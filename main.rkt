;;; @file main.rkt
;;; @brief Lexer for C# language
;;; @author Carlos Salguero
;;; @author Sergio Garnica
;;; @date 2023-04-03
;;; @version 1.0
;;; @copyright Copyright (c) 2023 - MIT License

#lang racket

;;; Libraries
(require racket/string)
(require racket/match)
(require racket/file)
(require racket/hash)

;;; @brief
;;; Reads a file and returns a list of lines
;;; @param file-name Name of the file to read
;;; @return List of lines
(define (read-file file-name)
    (open-input-file file-name)
)


;;; @brief
;;; Defining the constants for the lexer
(define keywords
  (list 
    "abstract" "as" "base" "bool" "break" "byte" "case" "catch" "char" "checked" "class" "const" "continue" "decimal" "default" "delegate" "do" "double" "else" "endif" "enum" "event" "explicit" "extern" "false" "finally" "fixed" "float" "for" "foreach" "goto" "if" "implicit" "in" "int" "interface" "internal" "is" "lock" "long" "namespace" "new" "null" "object" "operator" "out" "override" "params" "private" "protected" "public" "readonly" "ref" "return" "sbyte" "sealed" "short" "sizeof" "stackalloc" "static" "string" "struct" "switch" "this" "throw" "true" "try" "typeof" "uint" "ulong" "unchecked" "unsafe" "ushort" "using" "virtual" "void" "volatile" "while" "#if" "#endif" "#else"
   )
)

;;; @brief
;;; Defining the operators for the lexer
(define operators
    (list
        "+" "-" "*" "/" "%" "^" "&" "|" "~" "!" "=" "<" ">" "?" ":" ";" "," "." "++" "--" "&&" "||" "==" "!=" "<=" ">=" "+=" "-=" "*=" "/=" "%=" "^=" "&=" "|=" "<<=" ">>=" "=>" "??"
    )
)

;;; @brief
;;; Defining the delimiters for the lexer
(define delimiters
    (list
        "(" ")" "{" "}" "[" "]"
    )
)

;;; @brief
;;; Defining the colors for the lexer
(define colors
    (hash 
    "keyword" "keyword"
    "operator" "operator"
    "delimiter" "delimiter"
    "comment" "comment"
    "string" "string"
    "number" "number"
    "identifier" "identifier"
    )
)

;;; @brief
;;; Determines the token type of a given string
;;; @param token String to determine the type
;;; @return Token type (keyword, operator, delimiter, comment, 
;;;         string, number, identifier) or false if it is not a valid token
(define (classify-token token)
    (cond
        [(regexp-match #rx"//." token) "comment"]
        [(regexp-match #rx"/*.*/" token) "comment"]
        [(member token operators) "operator"]
        [(member token keywords) "keyword"]
        [(member token delimiters) "delimiter"]
        [(regexp-match? #rx"^\".*\"$" token) "string"]
        [(regexp-match? #rx"^[0-9x]+$" token) "number"]
        [(regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" token) "identifier"]
        [else #f]
    )
)

;;; @brief
;;; Highlights the given token with the appropriate color
;;; @param token Token to highlight
;;; @param token-type Type of the token
;;; @return Token highlighted with the appropriate color
(define (highlight-token token token-type)
    (cond
        [(equal? token-type "keyword") 
            (string-append "<span class=\""
            (hash-ref colors "keyword") "\">" token "</span>")]

        [(equal? token-type "operator") 
            (string-append "<span class=\""
            (hash-ref colors "operator") "\">" token "</span>")]

        [(equal? token-type "delimiter") 
            (string-append "<span class=\""
            (hash-ref colors "delimiter") "\">" token "</span>")]

        [(equal? token-type "comment") 
            (string-append "<span class=\""
            (hash-ref colors "comment") "\">" token "</span>")]
            
        [(equal? token-type "string") 
            (string-append "<span class=\""
            (hash-ref colors "string") "\">" token "</span>")]

        [(equal? token-type "number") 
            (string-append "<span class=\""
            (hash-ref colors "number") "\">" token "</span>")]

        [(equal? token-type "identifier") 
            (string-append "<span class=\""
            (hash-ref colors "identifier") "\">" token "</span>")]

        [else token]
    )
)

;;; @brief
;;; Tokenizes a given line
;;; @param line Line to tokenize
;;; @return List of tokens
(define (tokenize-line line open-block-comment)
    (define word '())
    (define list-line '())
    (define tokenized-line '())
    (define open-quotes #f)

    (define possible-line-comment #f)
    (define open-line-comment #f)

    ; Split the line into characters
    (define chars (regexp-split #px"" line))

     (for/last ([char chars])
      ; If no match is found in the last character, add the word to the list
      (when (and (eq? char (last chars)) (or open-line-comment open-block-comment))
        (set! list-line (append list-line (list word))))

      ; Match the character with the regular expressions
      (cond 
        [open-block-comment (set! word (append word (list char)))]

        [(regexp-match #rx"#" char) (set! word (append word (list char)))]

        [(regexp-match? #rx"[a-zA-Z0-9_]" char)
         (set! word (append word (list char)))]

        ; Match for line comments
        [(regexp-match #px"/" char) 
          (cond 
            [possible-line-comment 
              ((lambda () 
                (set! possible-line-comment #f)
                (set! open-line-comment #t)
                (set! word (append word (list char)))))]
            [else 
              ((lambda () 
                (set! possible-line-comment #t)
                (set! word (append word (list char)))))])]

        [open-line-comment (set! word (append word (list char)))]

        ; Match for strings
        [(regexp-match? #px"\"" char)
         (cond
           [open-quotes 
            ((lambda ()
               (set! open-quotes #f)
               (set! word (append word (list char)))
               (set! list-line (append list-line (list word)))
               (set! word '())))]
            [else ((lambda () 
              (set! open-quotes #t)
              (set! word (append word (list char)))))])]

        [open-quotes (set! word (append word (list char)))]

        ; Match for operators
        [(regexp-match? #px"[\\.\\,\\;\\(\\)\\{\\}\\[\\]\\=\\+\\-\\*\\/\\%\\>\\<\\:]" char)
         ((lambda ()
            (set! list-line (append list-line (list word)))
            (set! word '())
            (set! word (append word (list char)))
            (set! list-line (append list-line (list word)))
            (set! word '())))]

        ; Match for any other character
        [else
         ((lambda ()
            (set! list-line (append list-line (list word)))
            (set! word '())))])
    )

    (define tokens (map (lambda (x) (string-join x "")) list-line))

    (for ([token tokens])
        (define token-type (classify-token token))
        (when open-block-comment (set! token-type "comment"))
        (if token-type
            (set! tokenized-line (append tokenized-line 
                                 (list (highlight-token token token-type))))
            (set! tokenized-line (append tokenized-line (list token)))
        )
    )

    tokenized-line
)

;;; @brief
;;; Tokenizes a given file
;;; @param file-name Name of the file to tokenize
;;; @return List of lines with the tokens highlighted
(define (tokenize-file file-name)
    (let ((in-port (read-file file-name)))
        (let loop ((tokens '()))

        (let ((line (read-line in-port)))
            (if (eof-object? line)
                (reverse tokens)
                (loop (append tokens (tokenize-line line))))
            )
        )
    )
)


;;; @brief
;;; Writes a list of lines to a file
;;; @param file-name Name of the file to write
;;; @param lines List of lines to write
(define (write-file file-name lines)
    (define out (open-output-file file-name))

    (for ([line lines])
        (displayln line out)
    )

    (close-output-port out)
)

;;; @brief
;;; Main function
(define (main input-file output-file)
  (define input-lines (file->lines input-file))
  (define output-port (open-output-file output-file))
  (define html-header "<html><head><title>C# Lexer</title><link rel='stylesheet' href='./style.css' type='text/css' /></head><body>")
  (define html-footer "</body></html>")
  (write-string html-header output-port)

  (define open-block-comment #f)

  (for-each (lambda (line)
              (write-string (string-append "<pre>") output-port)

              (when (not open-block-comment) 
                  (set! open-block-comment (regexp-match? #px"/\\*" line)))
                  
              (define tokens (tokenize-line line open-block-comment))
              (define formatted-line (string-join tokens " "))

              (when open-block-comment
                  (set! open-block-comment (not (regexp-match? #px"\\*/" line))))

              (write-string (string-append formatted-line " ") output-port)
              (write-string (string-append "</pre>\n") output-port))
            input-lines)
  
  (write-string html-footer output-port)
  (close-output-port output-port))


;;; @brief
;;; Calling the main function
(main "index.cs" "index.html")