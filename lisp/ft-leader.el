;;; ft-leader.el -- Follow the Leader, a simple emacs leader -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'exwm)

(defun ft-leader-mode-upper-p (char)
  "Is the given CHAR upper case?"
  (and (>= char ?A) (<= char ?Z)))

(defun kbd-inverse (key)
  (if (not (sequencep key))
      (setq key (vector key)))
  (key-description key))

(defvar ft-main-map (make-sparse-keymap))
(defvar ft-priority-map (make-sparse-keymap))
(defvar ft-fallback-map (make-sparse-keymap))
(defvar ft-leader-mod-alist
  '((nil . "C-")
    ("m" . "M-")
    ("SPC" . "")

    ("<f9>" . "")
    ("RET" . "")
    ))

(defvar ft-nomod-command nil)

(defvar initial-key-string (cdr (assoc nil ft-leader-mod-alist)))
(defvar last-modifer-regex "[:space:]?\\([^[:space:]]+-$\\)")
(defvar ft-universal-arg nil)
(defvar ft-current-key-string nil)
(defvar ft-current-keymap nil)

(defun ft-format-universal-arg ()
  (let ((ft-universal-arg-str (substitute-command-keys "\\[universal-argument]")))
    (if ft-universal-arg
        (concat
         ft-universal-arg-str
         (pcase ft-universal-arg
           ('(-) "-")
           (`(,(and (pred integerp) n))
            (let ((str ""))
              (while (and (> n 4) (= (mod n 4) 0))
                (setq str (concat str " " ft-universal-arg-str))
                (setq n (/ n 4)))
              (if (= n 4) str (format " %s" ft-universal-arg))))
           (_ (format " %s" ft-universal-arg)))
         " ")
      "")))

(defun ft-leader-mode-exec (arg)
  (interactive "P")
  (setq ft-universal-arg arg)
  (setq ft-current-key-string initial-key-string)
  (setq ft-current-keymap (make-composed-keymap (current-active-maps)))

  (with-current-buffer
      (window-buffer (frame-selected-window exwm-workspace--current))
    (let ((char-mode-p
	   (and (derived-mode-p 'exwm-mode)
		(eq exwm--selected-input-mode 'char-mode))))
      (when char-mode-p
	(exwm-input--grab-keyboard exwm--id))

      (condition-case err
	  (cl-loop
	   (let* ((key
		   (read-key
		    (kbd-inverse
		     (kbd (concat (ft-format-universal-arg) ft-current-key-string)))))
		  (key-kbd (kbd-inverse key))

		  (key-mod (assoc key-kbd ft-leader-mod-alist))
		  (mod (when key-mod (cdr key-mod)))

		  (current-mod-p (string-suffix-p "-" ft-current-key-string))

		  (ft-universal-arg-str (ft-format-universal-arg)))

	     ;; When we don't have a current-mod-p and we've seleced the empty mod,
	     ;; select the initial-mod instead
	     ;; i.e. allows toggling ~C-~ with ~SPC~ when on a map
	     ;; try ~<leader> x SPC SPC SPC~ and watch the echo ~C-x C -~ and ~C-x ~
	     (when (and (string= "" mod) (not current-mod-p))
	       (setq mod initial-key-string))

	     ;; When we have a selected mod that matches the current-mod
	     ;; set the mod to nil. i.e. allows entering ~M-m~ with ~<leader> m m~
	     ;; if ~("m" . "M-")~ in alist
	     (if (string-match last-modifer-regex ft-current-key-string)
		 (let ((current-mod (match-string 0 ft-current-key-string)))
		   (if (string= mod current-mod)
		       (setq mod nil))))

	     ;; When we have a mod at this point, remove the existing mod and concat
	     (when mod
	       (let ((key-string-no-mod
		      (replace-regexp-in-string
		       last-modifer-regex "" ft-current-key-string nil 'literal)))
		 (setq key-kbd ""
		       ft-current-key-string (concat key-string-no-mod mod)))
	       ;; If selecting this mod leads us to an empty key-string, bail
	       ;; FIXME: should be configurable action
	       (when (string= "" ft-current-key-string)
		 (when (commandp ft-nomod-command)
		   (let ((current-prefix-arg ft-universal-arg))
		     (call-interactively ft-nomod-command)))
		 (when char-mode-p
		   (exwm-input--release-keyboard exwm--id))
		 (cl-return)))

	     ;; Set shift state based on key directly
	     (setq this-command-keys-shift-translated
		   (if (and (not (symbolp key))
			    (ft-leader-mode-upper-p key))
		       t nil))

	     ;; Look up the binding
	     (let* ((command-vec (kbd (concat ft-current-key-string key-kbd)))
		    (binding (lookup-key (current-active-maps) command-vec))
		    (main-binding (lookup-key ft-main-map command-vec))
		    (priority-binding (lookup-key ft-priority-map command-vec))
		    (fallback-binding (lookup-key ft-fallback-map command-vec))
		    (full-parsed-str
		     (kbd-inverse
		      (kbd (concat ft-universal-arg-str ft-current-key-string key-kbd)))))

	       ;; NOT YET IMPLEMENTED
	       ;; Check the current mod and build the inverse mod if the mod is either "" or the one assoced with nil
	       ;; If the inverse exists as a command or keymap, swap it in for the binding.
	       ;; This will allow seamless use of a top level char to capture the non-~C-~ version if one exists
	       ;; i.e. if ~C-c o~ is defined, and ~C-c C-o~ is not, ~<leader> c o~ will execute the binding at
	       ;; ~C-c o~. This also works in the inverse. ~<leader> c SPC o~ executes ~C-c C-o~ if it is defined
	       ;; and ~C-c o~ is not.



	       ;; Here we check for priority and fallback bindings based on only the final part of the
	       ;; command. This means that if ~C-x~ is defined as such in the ft-*-map, it will apply
	       ;; against every keymap, even beyond the global scope. i.e. we can bind ~C-g~ to 'keyboard-exit
	       ;; in our priority map, and every map will execute 'keyboard-exit with ~C-g~.
	       (if (string-match "[:space:]?\\([^[:space:]]+\\)[:space:]?$" full-parsed-str)
		   (let* ((match (match-string 0 full-parsed-str))
			  (current-command (kbd match)))
		     (if (not (commandp priority-binding))
			 (setq priority-binding (lookup-key ft-priority-map current-command)))
		     ;; Check again against the priority, and if it's still not a command, set the binding
		     ;; from the current-keymap.
		     (if (not (commandp priority-binding))
			 (setq priority-binding (lookup-key ft-current-keymap current-command)))
		     (if (not (commandp fallback-binding))
			 (setq fallback-binding (lookup-key ft-fallback-map current-command)))))

	       ;; When the binding is only one long, we check against the main-map and set that
	       ;; as priority if it exists
	       (when (and (= 1 (length command-vec))
			  (or (keymapp main-binding)
			      (commandp main-binding)))
		 (setq priority-binding main-binding))

	       ;; Here we set the binding again, checking the fallbacks and priorities.
	       (setq binding
		     (if (or (keymapp priority-binding)
			     (commandp priority-binding))
			 priority-binding
		       (if (and (or (keymapp fallback-binding)
				    (commandp fallback-binding))
				(not (or (keymapp binding)
					 (commandp binding))))
			   fallback-binding
			 binding)))

	       (cond
		;; if we have a mod, echo the str and loop
		(mod nil)

		;; in order to allow successive calls to universal-argument work as expected,
		;; i.e. entering ~<leader> u <leader> u <leader> x f~ is equivelent to ~C-u C-u C-x C-f~,
		;; 'universal-argument-more is substituted when arg is a cons.
		;; when arg is not a cons, the usual 'universal-argument is called interactively
		;; via the regular method below
		((and (commandp binding)
		      (eq binding 'universal-argument)
		      (consp ft-universal-arg))
		 (setq binding 'universal-argument-more)
		 (let ((current-prefix-arg ft-universal-arg))
		   (call-interactively binding))
		 (when char-mode-p
		   (exwm-input--release-keyboard exwm--id))
		 (cl-return))

		;; if we have a command, echo the str and call the binding with our prefix
		((commandp binding)
		 (setq this-command binding
		       this-original-command binding
		       real-this-command binding)
		 (let ((current-prefix-arg ft-universal-arg))
		   (call-interactively binding))
		 (when char-mode-p
		   (exwm-input--release-keyboard exwm--id))
		 (cl-return))

		;; if we have a keymap, build the next key-string and loop for more input
		((keymapp binding)
		 (setq ft-current-keymap binding)
		 (setq ft-current-key-string
		       (concat ft-current-key-string key-kbd " " initial-key-string)))

		;; Otherwise we have a not found
		(:else
		 (message "%s is not a command or keymap" full-parsed-str)
		 (when char-mode-p
		   (exwm-input--release-keyboard exwm--id))
		 (cl-return))))))

	(error
	 (progn
	   (when char-mode-p
	     (exwm-input--release-keyboard exwm--id))
	   (message "ft-leader loop error encountered -- bailing \n%s"
		    (error-message-string err))))))))

(provide 'ft-leader)

;;; ft-leader.el ends here
