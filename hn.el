;;; hackernews --- emacs hacker news client
;;; Commentary:
; functions for viewing hacker news posts
; the comment output is intented to be used with word wrap turned on
; and the adaptive wrap package
; TODO
; - default to enabling word wrap and adaptive wrap
; - collapsing comments
; - fix encoding issues
;;; Code:

(setq hn-api-base-url "http://node-hnapi.herokuapp.com/")

; see http://emacs.stackexchange.com/questions/12464/go-to-body-after-url-retrieve-synchronously
(defun get-url (url)
  "Return the body of the response from URL as a string (discards headers)."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (buffer-string)))

(defun get-json (url)
  "Return the body of the response from URL parsed as json."
  (json-read-from-string
   (get-url url)))

(defun hn-open-as-markdown (url)
  "Render the passed URL as markdown, using the fuckyeahmarkdown api."
  (switch-to-buffer
   (with-current-buffer (get-buffer-create "*hn-article*")
     (erase-buffer)
     (insert (get-url
	      (concat "http://fuckyeahmarkdown.com/go/?u="
		      (url-encode-url url))))
     (goto-char (point-min))
     (recode-region (point-min) (point-max) 'utf-8 'utf-8-unix)
     (current-buffer))))

(defun first (a)
  "Return the first item in the array, A."
  (aref a 0))

(defun hn-post-to-string (post)
  "Convert the POST object to a string representation."
  (concat
   "- " (hn-format-content 0 (cdr (assoc 'title post)))
   "\n  "
   "Score: " (number-to-string (or (cdr (assoc 'points post)) 0))
   " - "
   (cdr (assoc 'time_ago post))
   " - "
   (number-to-string (or (cdr (assoc 'comments_count post)) 0))
   " comments"
   "\n  " "(hn-open-as-markdown \"" (cdr (assoc 'url post)) "\")"
   "\n  " "(browse-url \"" (cdr (assoc 'url post)) "\")"
   "\n  " "(hn-view-comments " (number-to-string (or (cdr (assoc 'id post)) 0)) ")" ))

(defun hn-get-frontpage ()
  "Return a string representation of all the posts on the hacker news frontpage."
  (mapconcat
   'hn-post-to-string
   (get-json (concat hn-api-base-url "news"))
   "\n\n"))

;; For the life of me I can't figure out how to get the character encoding right
;; for this, so this is the hack we have to deal with...
;; This is also probably pretty inefficient as it does multiple passes through
;; the buffer, but I haven't noticed any performance issues.
(defun hn-format-content (padding content)
  "Format the content response from the hn api.
Will replace all the special characters in CONTENT and add PADDING to the
paragraph bodies."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (while (search-forward "<p>" nil t) (replace-match (concat "\n\n" padding) nil t))
    (goto-char (point-min))
    (while (search-forward "&#x2F;" nil t) (replace-match "/" nil t))
    (goto-char (point-min))
    (while (search-forward "&#x27;" nil t) (replace-match "'" nil t))
    (goto-char (point-min))
    (while (search-forward "&quot;" nil t) (replace-match "\"" nil t))
    (goto-char (point-min))
    (while (search-forward "\342\200\223" nil t) (replace-match "-" nil t))
    (goto-char (point-min))
    (while (search-forward "\342\200\230" nil t) (replace-match "`" nil t))
    (goto-char (point-min))
    (while (search-forward "\342\200\231" nil t) (replace-match "`" nil t))
    (goto-char (point-min))
    (while (search-forward "<a href=\"" nil t) (replace-match "["))
    (goto-char (point-min))
    (while (re-search-forward "\" rel=\"nofollow\">.*?</a>" nil t) (replace-match "]"))
    (goto-char (point-min))
    (while (search-forward "<i>" nil t) (replace-match "*"))
    (goto-char (point-min))
    (while (search-forward "</i>" nil t) (replace-match "*"))
    (goto-char (point-min))
    (while (search-forward "&lt;" nil t) (replace-match "<"))
    (goto-char (point-min))
    (while (search-forward "&gt;" nil t) (replace-match ">"))
    (buffer-string)))

(defun hn-format-comment (comment)
  "Pull the important parts out of a comment object and format them nicely.
Recursively do this for all child comments as well.
Expects COMMENT to be an object returned from the hacker news api."
  (let* ((depth (cdr (assoc 'level comment)))
	 (comments (cdr (assoc 'comments comment)))
	 (padding (make-string (* depth 4) ? ))
	 (content (hn-format-content padding (cdr (assoc 'content comment))))
	 (user (cdr (assoc 'user comment)))
	 (time-ago (cdr (assoc 'time_ago comment))))
    (concat ; "\n"
	    content
	    "\n" padding "--- " user " - " time-ago
	    (mapconcat 'hn-format-comment comments ""))))

(defun hn-get-comments (item-id)
  "Return the comments array for the given ITEM-ID."
  (cdr (assoc 'comments (get-json
			 (concat hn-api-base-url
				 "item/"
				 (number-to-string item-id))))))

(defun hn-view-comments (item-id)
  "Insert the comments for the post with the given ITEM-ID into the hacker news buffer."
  (switch-to-buffer
   (with-current-buffer (get-buffer-create "*hn-comments*")
     (erase-buffer)
     (insert (concat "Viewing item id # " (number-to-string item-id)))
     (insert (mapconcat 'hn-format-comment (hn-get-comments item-id) "\n"))
     (goto-char (point-min))
     (current-buffer))))

(defun hackernews ()
  "Create a buffer if it doesn't exist, and put the hn front page there."
  (interactive)
  (with-current-buffer (get-buffer-create "*hn*")
    (erase-buffer)
    (insert (hn-get-frontpage))
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

(provide 'hn)
;;; hn.el ends here
