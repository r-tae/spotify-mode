;;; spotify.el -- control Spotify from a pop-up window -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 River Smith
;;
;; Author: River Smith <http://github/river>
;; Maintainer: River Smith <riversmith@protonmail.com>
;; Created: July 06, 2020
;; Modified: July 06, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/river/spotify
;; Package-Requires: ((emacs 27.0.91) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'url)
(require 'evil)

(defun spotify ()
  "Control spotify."
  (interactive)
  (display-buffer
   (get-buffer-create "spotify")
   '(display-buffer-below-selected
     "spotify"
     (window-height . 10) (window-width . 40))
   )
  (switch-to-buffer-other-window "spotify")
  (spotify-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "This will be some controls"))
  (spotify-api-init)
  )

(defun spotify-parse-json (path json)
  "Internal - do not use.
PATH: list of ints/strings, to access nested json
JSON: list/hash-table"
  (if path
      (pcase (type-of json)
        ('hash-table
         (spotify-parse-json (cdr path) (gethash (car path) json)))
        ('vector
         (spotify-parse-json (cdr path) (aref json (car path))))
        ;;('cons
        ;; (spotify-parse-json path (car json)))
        (_ json))
    json
    ))

(defvar spotify-access-token "")
(defvar spotify-state
  `((playing . paused)
    (artist . "No one")
    (album . "None")))

(defconst spotify-api-base "https://api.spotify.com/v1")
(defconst spotify-api-methods
  `((current .
             (
              (method . "GET")
              (url . ,(concat spotify-api-base "/me/player"))
              ))
    (play .
          (
           (method . "PUT")
           (url . ,(concat spotify-api-base "/me/player/play"))
           ))
    (pause .
           (
            (method . "PUT")
            (url . ,(concat spotify-api-base "/me/player/pause"))
            ))
    (next .
          (
           (method . "POST")
           (url . ,(concat spotify-api-base "/me/player/next"))
           ))
    (previous .
              (
               (method . "POST")
               (url . ,(concat spotify-api-base "/me/player/previous"))
               ))
    ))
(defconst spotify-api-player-info "https://api.spotify.com/v1/me/player")
(defconst spotify-api-url-auth "https://accounts.spotify.com/authorize")

(defconst spotify-api-client-id "43f610286d18421d86a50f8d11476c03")
(defconst spotify-api-client-secret "9dd26d3e955f4c408cc91870d2a05b49")

(defun spotify-print-buffer ()
  "Update the spotify buffer."
  (let ((inhibit-read-only t))
    (switch-to-buffer "spotify")
    (erase-buffer)
    (pcase (cdr (assoc 'playing spotify-state))
      ('paused
       (insert "Stopped"))
      ('playing
       (insert "Playing")))))

(defun spotify-download-temp-image (path)
  "Download PATH as a temp file and return it."
  (let ((img (expand-file-name
              (concat (md5 path) "." (or (file-name-extension path) "jpeg"))
              temporary-file-directory)))
    (if (file-exists-p img)
        img
      (url-copy-file path img)
      img)))

(defun spotify-get-album-art (json)
  "Image ready to be inserted into a buffer from JSON returned from spotify."
  (let* ((url (spotify-parse-json '("item" "album" "images" 1 "url") json))
         (image (spotify-download-temp-image url)))
    (create-image image nil nil :scale 0.4)))

(defun spotify-get-album-name (json)
  "Album name from Spotify JSON payload."
  (spotify-parse-json '("item" "album" "name") json))

(defun spotify-load-state ()
  "Internal use only."
  (let* ((json (spotify-make-api-call 'current))
         (inhibit-read-only t)
         (image (spotify-download-temp-image
                 (spotify-parse-json
                  '("item" "album" "images" 1 "url") json))))
    (switch-to-buffer "spotify")
    (insert-image (create-image image nil nil :scale 0.4))))

(defun spotify-current ()
  "Go back a track."
  (interactive)
  (spotify-make-api-call 'current))

(defun spotify-pause-play ()
  "Toggle playing state."
  (interactive)
  (pcase (cdr (assoc 'playing spotify-state))
    ('paused
     (spotify-make-api-call 'play)
     (setf (cdr (assoc 'playing spotify-state)) 'playing))
    ('playing
     (spotify-make-api-call 'pause)
     (setf (cdr (assoc 'playing spotify-state)) 'paused)))
  (spotify-print-buffer))

(defun spotify-next ()
  "Go back a track."
  (interactive)
  (spotify-make-api-call 'next))

(defun spotify-previous ()
  "Go back a track."
  (interactive)
  (spotify-make-api-call 'previous))

(defun spotify-make-api-call (action)
  "Internal - use one of the call specific methods.
ACTION: either POST or GET"
  (let* ((endpoint (assoc action spotify-api-methods))
         (access-token-set (not (string-equal spotify-access-token "")))
         (auth-code (concat "Bearer " spotify-access-token))
         (url-http-end-of-headers t)
         (url-request-method (cdr (assoc 'method endpoint)))
         (url-request-extra-headers
          (list
           (cons "Authorization" auth-code)
           (cons "Content-Length" "0")
           )))
    (if access-token-set
        (progn
          (set-buffer (url-retrieve-synchronously (cdr (assoc 'url endpoint))))
          (let ((payload (delete-and-extract-region
                          (point)
                          (point-max))
                         ))
            (json-parse-string payload)
            )))))

(defun spotify-api-init ()
  "Make a call to the Spotify APIs."
  (let ((inhibit-read-only t))
    (browse-url
     (concat
      spotify-api-url-auth
      "?client_id="
      spotify-api-client-id
      "&response_type=token"
      "&scope=user-modify-playback-state%20user-read-playback-state%20user-read-currently-playing"
      "&redirect_uri=https://svelte-gold.vercel.app"))
    (setq spotify-access-token
          (read-from-minibuffer "Please paste your auth code here: "))
    ))

;;(defvar spotify-mode-map
;;  (let ((map (make-sparse-keymap)))
;;    (define-key map (kbd "k") 'spotify-play)
;;    (define-key map (kbd "l")  'spotify-next)
;;    (define-key map (kbd "j") 'spotify-previous)
;;    map)
;;  "Keymap for Spotify mode.")

(defvar spotify-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (define-key map (kbd "k") #'spotify-pause-play)
    (define-key map (kbd "l") #'spotify-next)
    ;; Set parent map for foo-mode-map:
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for foo-mode.")

(define-derived-mode spotify-mode special-mode "spotify"
  "Major mode for controlling Spotify.

\\{spotify-mode-map}")

(provide 'spotify)
;;; spotify.el ends here
