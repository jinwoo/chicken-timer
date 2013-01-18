(require-extension posix)
(require-extension srfi-37)


;; App title.
(define APP-TITLE "Timer")


;; The notifier app.
(define NOTIFIER-APP
  "./terminal-notifier.app/Contents/MacOS/terminal-notifier")


;; Program name.
(define PROGRAM-NAME
  (pathname-file (program-name)))


;;;;;;;;;;
;; Command line options.

;; Option for a pomodoro.
(define pomodoro
  (option
   '(#\p "pomodoro") #f #f
   (lambda (o n x vals)
     (cons 'pomodoro vals))))


;; Option for a short break.
(define short-break
  (option
   '(#\s "short-break") #f #f
   (lambda (o n x vals)
     (cons 'short-break vals))))


;; Option for a short break.
(define long-break
  (option
   '(#\l "long-break") #f #f
   (lambda (o n x vals)
     (cons 'long-break vals))))


;; Option for help.
(define help
  (option
   '(#\h "help") #f #f
   (lambda (o n x vals)
     (cons 'help vals))))


;; All options.
(define ALL-OPTIONS
  (list pomodoro
        short-break
        long-break
        help))


;; All options with descriptions.
(define ALL-OPTIONS-WITH-DESCRIPTIONS
  (list (cons pomodoro "Start a pomodoro.")
        (cons short-break "Take a short break.")
        (cons long-break "Take a long break.")
        (cons help "Show this text.")))


;; Prints usage and exits.
(define (usage)
  (printf "Usage: ~a OPTIONS~n" PROGRAM-NAME)
  (for-each
   (lambda (opt-with-description)
     (define opt (car opt-with-description))
     (define description (cdr opt-with-description))
     (define names (option-names opt))
     (define option-strings (format-option-strings names))
     (printf "  ~a: ~a~n" option-strings description))
   ALL-OPTIONS-WITH-DESCRIPTIONS)
  (exit 1))


;; Formats the option strings for use with USAGE.
;; OPT-NAMES is a list of strings.
(define (format-option-strings opt-names)
  (foldl
   (lambda (acc name)
     (string-append acc
                    " "
                    (cond
                     ((char? name) (sprintf "-~a" name))
                     ((string? name) (sprintf "--~a" name)))))
   ""
   opt-names))


;; Parsed options.
(define parsed-options
  (args-fold (command-line-arguments)
             ALL-OPTIONS
             (lambda (o n x vals)
               (fprintf (current-error-port) "Unrecognized option: ~a~n" n)
               (usage))
             cons
             '()))


;; Gets the full path of the notifier app.
(define (get-notifier-app-full-path)
  (define my-path (get-app-path))
  (define path
    (if my-path
        (string-append my-path "/" NOTIFIER-APP)
        NOTIFIER-APP))
  (normalize-pathname path))


;; Gets the full path of the application file.
(define (get-app-path)
  (pathname-directory (program-name)))


;; Alerts after MINUTES using MESSAGE.
;; MINUTES is a number, MESSAGE is a string.
(define (alert-after minutes message)
  (define start-time (current-seconds))
  (define end-time (+ start-time (* minutes 60)))
  (define (loop)
    (define now (current-seconds))
    (define remaining-seconds (max 0 (- end-time now)))
    (display-remaining remaining-seconds)
    (if (>= now end-time)
        (begin
          (newline)
          (notify message))
        (begin
          (sleep 1)
          (loop))))
  (loop))


;; Display remaining time.
;; REMAINING-SECONDS is a number
(define (display-remaining remaining-seconds)
  (define mins (inexact->exact
                (quotient remaining-seconds 60)))
  (define secs (inexact->exact
                (remainder remaining-seconds 60)))
  (define secs-string (format-seconds secs))
  (status-message "Time left: ~a:~a" mins secs-string))


;; Pads with 0 if the number of digits of SECS is < 2.
(define (format-seconds secs)
  (define str (number->string secs))
  (if (< (string-length str) 2)
      (string-append "0" str)
      str))


;; Formats and displays the given string, by overwriting the previously
;; displayed one.
(define (status-message format-string . args)
  (define clear-message (string-append "\r" (make-string 80 #\space)))
  (define fmt (string-append clear-message "\r" format-string))
  (apply printf fmt args)
  (flush-output))


;; Shows the notification MESSAGE.
;; MESSAGE is a string.
(define (notify message)
  (define command
    (sprintf "~a -title ~a -message \"~a\""
             (get-notifier-app-full-path) APP-TITLE message))
  (let-values (((in out pid) (process command)))
    (process-wait pid)))


;; Returns the string representing the current time.
(define (current-time-string)
  (seconds->string (current-seconds)))


;; Main.
(define (main)
  (cond
   ((member 'pomodoro parsed-options)
    (printf "[~a] Starting a pomodoro.~n" (current-time-string))
    (alert-after 25 "You're done!  Take a break."))
   ((member 'short-break parsed-options)
    (printf "[~a] Starting a short break.~n" (current-time-string))
    (alert-after 5 "Break is over.  Get back to work."))
   ((member 'long-break parsed-options)
    (printf "[~a] Starting a long break.~n" (current-time-string))
    (alert-after 15 "Break is over.  Get back to work."))
   (else (usage))))


(main)
