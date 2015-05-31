;;; Hack-Assembler --- Convert .asm to .hack -*- lexical-binding: t -*-

;;; Commentary:

;; This is my solution to the laste exercise of the Nand 2 Tetris course
;; from coursera.com. It's an assembler for the very simple symbolic
;; language defined in the course, but it has nice features like automatic
;; variable references and tags to mark program addresses. You can see some
;; examples in the 06/ folder, along with the generated .hack files.

;; I chose to solve this exercise in Emacs Lisp to seize the opportunity to
;; learn a bit about this really unique (in this days) language. It was an
;; interesting exercise, no doubts about it. I leave with hunger for more.
;; But, truth be told, the language shows its (ancient) age. Which is cool,
;; in a way. Like a relic from the past. It shows how powerful this primitive
;; lisps really were, and the whole experience this intangible lisp mystique.
;; I liked it very much :)

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; config and constants

(defvar *operations-table*
  (apply #'append
         '((\0 "0101010") (\1 "0111111") (\-1 "0111010") (D "0001100")
           (A "0110000") (!D "0001101") (!A "0110001") (-D "0001111")
           (-A "0110011") (D+1 "0011111") (A+1 "0110111") (D-1 "0001110")
           (A-1 "0110010") (D+A "0000010") (D-A "0010011") (A-D "0000111")
           (D&A "0000000") (D|A "0010101") (M "1110000") (!M "1110001")
           (M+1 "1110111") (M-1 "1110010") (D+M "1000010") (D-M "1010011")
           (M-D "1000111") (D&M "1000000") (D|M "1010101"))))

(defvar *destinations-table*
  (apply #'append
         '((nil "000") (M "001") (D "010") (MD "011")
           (A "100") (AM "101") (AD "110") (AMD "111"))))

(defvar *jumps-table*
  (apply #'append
         '((nil "000") (JGT "001") (JEQ "010") (JGE "011")
           (JLT "100") (JNE "101") (JLE "110") (JMP "111"))))

(defvar *symbol-constants*
  (apply #'append
         '(("SP" 0) ("LCL" 1) ("ARG" 2) ("THIS" 3) ("THAT" 4) ("R0" 0) ("R1" 1)
           ("R2" 2) ("R3" 3) ("R4" 4) ("R5" 5) ("R6" 6) ("R7" 7) ("R8" 8)
           ("R9" 9) ("R10" 10) ("R11" 11) ("R12" 12) ("R13" 13) ("R14" 14)
           ("R15" 15) ("SCREEN" 16384) ("KBD" 24576))))

;; reading functions

(defun read-file-as-string (path)
  "Read a file (PATH) as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun asm-linep (line)
  "Test if LINE is an actual instruction."
  (not (or (string-match "//" line)
           (string-match "^\s*$" line))))

(defun clean-line (line)
  "Trim whitespace and comments around the mnemonic in LINE."
  (replace-regexp-in-string "\s" ""
                            (replace-regexp-in-string "//.*$" "" line)))

(defun get-program-from-file (path)
  "Read the asm code of PATH and discards all comments + whitespace."
  (let* ((raw-string (read-file-as-string path))
         (raw-lines (split-string raw-string "\n")))
    (seq-filter #'asm-linep
                (mapcar #'clean-line raw-lines))))

(defun get-program-from-buffer (buf)
  "Read the asm code from the buffer BUF discarding comments + whitespace."
  (with-current-buffer buf
    (let* ((raw-text (buffer-substring-no-properties (point-min) (point-max)))
           (raw-lines (split-string raw-text "\n")))
      (seq-filter #'asm-linep
                  (mapcar #'clean-line raw-lines)))))

;; Instruction Predicates

(defun a-instructionp (mnemonic)
  "Test if MNEMONIC is a instruction of type A."
  (string-match "^@" mnemonic))

(defun is-symbolp (name)
  "Test if NAME is a symbol name."
  (null (string-match "^[0-9]+$" name)))

(defun has-symbolp (mnemonic)
  "Test if a symbol is present in MNEMONIC."
  (when (a-instructionp mnemonic)
    (is-symbolp (substring mnemonic 1))))

(defun label-instructionp (mnemonic)
  "Test if MNEMONIC is a label declaration."
  (string-match "^(.+)$" mnemonic))

;; Symbol environment

(defun make-symbol-table ()
  "Generate new symbol plist."
  (copy-tree *symbol-constants*))

(defun make-environment ()
  "Generate new environment."
  ;; variables starts on address 0x0010
  (list 'table (make-symbol-table) 'pointer #x0010))

(defun env-add-symbol (env symbol &optional rom-addr)
  "Expands ENV with SYMBOL (to the env pointer or to ROM-ADDR)."
  (let* ((symbol-table (plist-get env 'table))
         (addr (or rom-addr
                   (plist-get env 'pointer)))
         (new-symbol-table (append symbol-table
                                   (list symbol addr))))
    (plist-put env 'table new-symbol-table)))

(defun env-increment-pointer (env)
  "Increment the variable pointer in ENV."
  (plist-put env 'pointer
             (1+ (plist-get env 'pointer))))

(defun env-get-symbol (env symbol)
  "Get from ENV the value of SYMBOL (or silent nil)."
  (let ((symbol-table (plist-get env 'table)))
    (lax-plist-get symbol-table symbol)))

(defalias 'env-has-symbolp #'env-get-symbol "Test if ENV knows about SYMBOL.")

(defun env-add-variable (variable-name env)
  "Add the variable VARIABLE-NAME to ENV."
  (if (not (env-has-symbolp env variable-name))
      (env-increment-pointer (env-add-symbol env variable-name))
    env))

(defun env-resolve-symbol (env symbol)
  "Get from ENV the value of SYMBOL (or throw)."
  (or (env-get-symbol env symbol)
      (throw 'UNKNOWN-SYMBOL symbol)))

;; First pass

(defun extract-label-name (mnemonic)
  "Extracts the label name from MNEMONIC."
  (substring mnemonic 1 -1))

(defun first-pass (program env)
  "Read all labels from PROGRAM and save the symbols in ENV."
  (let ((line-number 0))
    (cl-reduce (lambda (env mnemonic)
                 (if (label-instructionp mnemonic)
                     (env-add-symbol env (extract-label-name mnemonic)
                                     line-number)
                   (prog1 env
                     (setq line-number (1+ line-number)))))
               program :initial-value env)))

(defun remove-labels (program)
  "Remove all label pseudo-ops from PROGRAM."
  (cl-remove-if #'label-instructionp program))

;; Second pass

(defun extract-variable-name (mnemonic)
  "Extracts the variable name from MNEMONIC."
  (substring mnemonic 1))

(defun maybe-add-variable (env mnemonic)
  "Extend ENV with a var symbol if present in MNEMONIC."
  (if (has-symbolp mnemonic)
      (env-add-variable (extract-variable-name mnemonic) env)
    env))

;; A-instructions

(defun number-to-binary (n &optional res)
  "Convert N to it's binary string representation RES."
  (if (= n 0)
      (or res "0")
    (number-to-binary (lsh n -1)
                      (concat (if (= 1 (logand n 1)) "1" "0")
                              (or res "")))))

(defun pad (str n)
  "Add enough 0 to pad STR until it reaches N chars."
  (substring (concat (make-string n ?0) str) (- n)))

(defun resolve-direction (number-or-symbol env)
  "Convert NUMBER-OR-SYMBOL to a number, resolving it with ENV if needed."
  (if (is-symbolp number-or-symbol)
      (env-resolve-symbol env number-or-symbol)
    (string-to-number number-or-symbol)))

(defun assemble-a-instruction (mnemonic env)
  "Assemble MNEMONIC as a instruction of type A using ENV for symbols."
  (let ((number (resolve-direction (substring mnemonic 1) env)))
    (concat "0" (pad (number-to-binary number) 15))))

;; C-instructions

(defun c-has-destinationp (mnemonic)
  "Test if MNEMONIC has a destionation designator."
  (string-match "=" mnemonic))

(defun c-break-destination (mnemonic)
  "Break MNEMONIC into (rest destination)."
  (reverse (split-string mnemonic "=")))

(defun c-get-destination (mnemonic)
  "Extract the destination part of MNEMONIC."
  (when (c-has-destinationp mnemonic)
    (cadr (c-break-destination mnemonic))))

(defun c-has-jumpp (mnemonic)
  "Test if MNEMONIC has a jump designator."
  (string-match ";" mnemonic))

(defun c-break-jump (mnemonic)
  "Break MNEMONIC into (rest jump)."
  (split-string mnemonic ";"))

(defun c-get-jump (mnemonic)
  "Extract the jump part of MNEMONIC."
  (when (c-has-jumpp mnemonic)
    (cadr (c-break-jump mnemonic))))

(defun c-get-operation (mnemonic)
  "Extract the operation part of MNEMONIC."
  (let ((without-dest (car (c-break-destination mnemonic))))
    (car (c-break-jump without-dest))))

(defun assemble-c-preamble ()
  "Return the preamble for a C instruction."
  "111")

(defun c-translate-operation (op)
  "Convert OP in its binary representation."
  (plist-get *operations-table* (intern op)))

(defun assemble-c-operation (mnemonic)
  "Assemble the operation part of MNEMONIC."
  (c-translate-operation (c-get-operation mnemonic)))

(defun c-translate-destination (dest)
  "Convert DEST in its binary representation."
  (plist-get *destinations-table* (and dest (intern dest))))

(defun assemble-c-destination (mnemonic)
  "Assemble the destination part of MNEMONIC."
  (c-translate-destination (c-get-destination mnemonic)))

(defun c-translate-jump (jump)
  "Convert JUMP in its binary representation."
  (plist-get *jumps-table* (and jump (intern jump))))

(defun assemble-c-jump (mnemonic)
  "Assemble the jump part of MNEMONIC."
  (c-translate-jump (c-get-jump mnemonic)))

(defun assemble-c-instruction (mnemonic)
  "Assemble MNEMONIC as a instruction of type C."
  (concat (assemble-c-preamble)
          (assemble-c-operation mnemonic)
          (assemble-c-destination mnemonic)
          (assemble-c-jump mnemonic)))

;; assembler functions

(defun assemble-instruction (env mnemonic)
  "Assembles with symbols ENV the instruction MNEMONIC."
  (if (a-instructionp mnemonic)
      (assemble-a-instruction mnemonic env)
    (assemble-c-instruction mnemonic)))

(defun assemble-program (lines)
  "Assembles the seq of instructions in LINES."
  (let* ((env (first-pass lines (make-environment)))
         (program (remove-labels lines)))
    (mapcar (lambda (mnemonic)
              (setq env (maybe-add-variable env mnemonic))
              (assemble-instruction env mnemonic))
            program)))

(defun assemble-file (path)
  "Return the assembled text of the program on the file in PATH."
  (string-join (assemble-program (get-program-from-file path)) "\n"))

(defun assemble-buffer (buf)
  "Return the assembled text of the program on the buffer BUF."
  (string-join (assemble-program (get-program-from-buffer buf)) "\n"))

(defun assemble-current-buffer ()
  "Asseble the program in the current buffer."
  (interactive)
  (let ((program (assemble-buffer (current-buffer)))
        (output (get-buffer-create "*assember-output*")))
    (with-current-buffer output
      (erase-buffer)
      (insert program))
    (switch-to-buffer-other-window output)))

(provide 'assembler)
;;; assembler.el ends here
