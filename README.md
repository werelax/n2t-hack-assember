# Implementing a simple assembler in Emacs Lisp

This is my solution to the laste exercise of the [Nand 2 Tetris](https://class.coursera.org/nand2tetris1-001) course from coursera.com. It's an assembler for the very simple symbolic language defined in the course, but it has nice features like automatic variable references and tags to mark program addresses. You can see some examples in the 06/ folder, along with the generated .hack files.

I chose to solve this exercise in Emacs Lisp to seize the opportunity to learn a bit about this really unique (for these days) language. It was an interesting exercise, no doubts about it. I leave with hunger for more. But, truth be told, the language shows its (ancient) age. Which is cool, in a way. Like a relic from the past. It shows how powerful this primitive lisps really were, and the whole experience this intangible lisp mystique.

It's also worth mentioning how good the *programming-emacs-in-elisp* workflow is. Emacs is an amazing pieze of software. The oddities of the *elisp* language are compensated with the interesting mechanics the editor gives you. For example, to "execute" the assembler, you can just add the `.asm` file in a buffer and then `M-x assemble-current-buffer`. That's it! You don't even need to save the source.

Even if the language feels like it was designed by a small group or really old aliens, you can feel the renewal efforts behind it. The fantastic `cl-lib` for functional utilities and `subr-x` for string manipulation have been *really* helpful. And the *lexical scoping*. That's a BIG WIN (we `M-x prayed` our gods everyday till they gave us the *Closure*). AFAIK, even more and deeper improvements are (slowly) coming to the language. Yay!

All in all, I liked it very much :)
