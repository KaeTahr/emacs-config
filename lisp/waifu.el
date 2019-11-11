;;; waifu.el --- Have a comforting GNU/Emacs experience

;;; Commentary:

;;  You've heard of waifu.py.
;;  Now, we Emacs now.

;;; Code:

;; let's seed the random function, can't guarantee the user will.
(random t)

(defvar waifu-name 'nil
  "Name of your waifu.")
(defvar weeb-name 'nil
  "Name for the dumbass wanting to be comforted.")

(defvar quote-list (list "waifu-name comforts weeb-name as he gets ready for work."
			 "weeb-name puts his head on waifu-name's lap as he waits for Emacs to load."
			 "waifu-name: M-x pat-head RET weeb-name"
			 "waifu-name pats weeb-name's head as he desperately spams ESC to fix his fuckup."
			 "waifu-name: 'Don't forget to C-x C-s, weeb-name!'"
			 "waifu-name plays with weeb-name's hair as he opens another buffer."
			 "weeb-name pulls waifu-name close while he compiles his project again."
			 "waifu-name brings weeb-name a cup of tea to help him with the debugging."
			 "waifu-name quickly continues her Dunnet run while weeb-name is afk."
			 "waifu-name M-x butterfly's when weeb-name isn't looking."
			 "waifu-name adds lots of hugging sessions to weeb-name's org-agenda."
			 )
  "List of waifu quotes."
  )

(defun replace-placeholders (waifu-quote)
  "Replace the placeholders in WAIFU-QUOTE with the selected names."
  (replace-regexp-in-string "weeb-name" weeb-name (replace-regexp-in-string "waifu-name" waifu-name waifu-quote))
  )

(defun get-random-quote ()
  "Out of the list of quotes, randomly select one."
  (nth (random (length quote-list)) quote-list)
  )

(defun get-comfort ()
  "Get a non-placeholder'd quote."
      (replace-placeholders (get-random-quote))
  )

;; interactive functions
(defun pat-head ()
  "This is just for fun."
  (interactive)
  (message "Here you go, %s; pat, pat." (read-string "Headpats' Recipient: ")))
(defun comfort ()
  "Get comforted by your waifu from your Emacs command line."
  (interactive)
  (message (get-comfort))
  )


(provide 'waifu)
;;; waifu.el ends here
