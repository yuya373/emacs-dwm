;;; dwm.el --- tiled window manager for emacs        -*- lexical-binding: t; -*-

;; Copyright (C) 2016  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defgroup dwm-mode nil
  "Tiled window manager"
  :prefix "dwm-"
  :group 'convenience)

(defcustom dwm-ignore-buffers-regexp '("helm")
  "Ignore buffers name regexp")

(defun dwm-window-edges-alist ()
  (mapcar #'(lambda (win) (cons (window-edges win) win))
          (window-list-1)))

(defun dwm-win-left-pos (win-edge)
  (car win-edge))

(defun dwm-win-top-pos (win-edge)
  (cadr win-edge))

(defun dwm-win-right-pos (win-edge)
  (caddr win-edge))

(defun dwm-win-bottom-pos (win-edge)
  (cadddr win-edge))

(cl-defun dwm-find-window (&key left-pos top-pos right-pos bottom-pos)
  (cdr
   (cl-find-if
    #'(lambda (edge-win)
        (let ((edge (car edge-win)))
          (and (if left-pos (eq left-pos (dwm-win-left-pos edge)) t)
               (if top-pos (eq top-pos (dwm-win-top-pos edge)) t)
               (if right-pos (eq right-pos (dwm-win-right-pos edge)) t)
               (if bottom-pos (eq bottom-pos (dwm-win-bottom-pos edge)) t))))
    (dwm-window-edges-alist))))

(defun dwm-find-next-window (cur-win)
  (let ((main-win (dwm-main-window)))
    (if (equal cur-win main-win)
        (dwm-first-sub-window)
      (let ((next-win
             (cdr (cl-find-if #'(lambda (edge-win)
                                  (< (dwm-win-top-pos (window-edges cur-win))
                                     (dwm-win-top-pos (car edge-win))))
                              (dwm-sub-windows)))))
        (if next-win
            next-win
          main-win)))))

(defun dwm-find-prev-window (cur-win)
  (let ((main-win (dwm-main-window)))
    (if (equal cur-win main-win)
        (dwm-last-sub-window)
      (let ((prev-win
             (cdr (cl-find-if #'(lambda (edge-win)
                                  (> (dwm-win-bottom-pos
                                      (window-edges cur-win))
                                     (dwm-win-bottom-pos (car edge-win))))
                              (reverse (dwm-sub-windows))))))
        (if prev-win
            prev-win
          main-win)))))

(defun dwm-main-window ()
  (dwm-find-window :left-pos 0 :top-pos 0))

(defun dwm-sub-windows ()
  (cl-sort
   (remove-if #'(lambda (edge-win) (equal (cdr edge-win) (dwm-main-window)))
              (dwm-window-edges-alist))
   #'< :key #'(lambda (edge-win) (dwm-win-top-pos (car edge-win)))))

(defun dwm-first-sub-window ()
  (let ((main-win-edges (window-edges (dwm-main-window))))
    (dwm-find-window :left-pos (dwm-win-right-pos main-win-edges)
                     :top-pos (dwm-win-top-pos main-win-edges))))

(defun dwm-last-sub-window ()
  (cdr (car (last (dwm-sub-windows)))))

(defun dwm-create-sub-buffer (buffer)
  (let ((sub-window (split-window (dwm-main-window) nil 'right)))
    (set-window-buffer sub-window buffer)))

(defun dwm-load-sub-buffer (buffer)
  (let ((sub-window (dwm-first-sub-window)))
    (if sub-window
        (progn
          (condition-case _e
              (split-window sub-window window-min-height 'above)
            (error (let ((last-sub-win (dwm-last-sub-window)))
                     (delete-window last-sub-win)
                     (balance-windows)
                     (split-window (dwm-first-sub-window) window-min-height 'above))))
          (set-window-buffer (dwm-first-sub-window) buffer))
      (dwm-create-sub-buffer buffer))))

(defun dwm-delete-duplicated-buffer (buf)
  (delete-windows-on buf)
  ;; (let ((sub-buffers (mapcar #'(lambda (edge-win)
  ;;                                (cons (window-buffer (cdr edge-win))
  ;;                                      (cdr edge-win)))
  ;;                            (dwm-sub-windows))))
  ;;   (let ((buf-win
  ;;          (cdr
  ;;           (cl-find-if #'(lambda (sub)
  ;;                           (string= (buffer-name buf)
  ;;                                    (buffer-name (window-buffer (cdr sub)))))
  ;;                       (dwm-sub-windows)
  ;;                       :from-end t))))
  ;;     (if buf-win
  ;;         (delete-window buf-win))))
  )

(defun dwm-match-ignore-p (buf)
  (if (cl-find-if #'(lambda (regex)
                      (string-match regex (buffer-name buf)))
                  dwm-ignore-buffers-regexp)
      t))

(defun dwm--load-buffer (win loading-buf &optional before-win-buf)
  (unless (and before-win-buf
               (equal (buffer-name loading-buf)
                      (buffer-name before-win-buf)))
    (select-window win)
    (save-selected-window
      (dwm-delete-duplicated-buffer loading-buf)
      (set-window-buffer win loading-buf)
      (if before-win-buf
          (dwm-load-sub-buffer before-win-buf))
      (balance-windows))
    (set-buffer loading-buf)))

(defun dwm-load-buffer ()
  (interactive)
  (let* ((buf (current-buffer))
         (main-win (dwm-main-window))
         (win-buf (window-buffer main-win)))
    (unless (equal main-win (selected-window))
      (dwm--load-buffer main-win buf win-buf))))

(defun dwm-switch-to-buffer (org-func buffer-or-name &rest args)
  (let* ((main-window (dwm-main-window))
         (win-buf (window-buffer main-window))
         (loading-buf (get-buffer-create buffer-or-name)))
    (if (dwm-match-ignore-p loading-buf)
        (apply org-func buffer-or-name args)
      (dwm--load-buffer main-window
                        loading-buf
                        win-buf))))

(defun dwm-next-buffer ()
  (interactive)
  (select-window (dwm-find-next-window (selected-window))))

(defun dwm-prev-buffer ()
  (interactive)
  (select-window (dwm-find-prev-window (selected-window))))

(defun dwm-continue-main-window (org-func &optional window)
  (let ((win (or window (selected-window))))
    (if (equal win (dwm-main-window))
        (dwm--load-buffer win (window-buffer (dwm-first-sub-window)))
      (funcall org-func window))))

(define-minor-mode dwm-mode
  "Enable tiled window manage"
  nil nil nil
  (if dwm-mode
      (progn
        (advice-add 'delete-window :around 'dwm-continue-main-window)
        (advice-add 'switch-to-buffer-other-window :around 'dwm-switch-to-buffer)
        (advice-add 'switch-to-buffer :around 'dwm-switch-to-buffer)
        (advice-add 'pop-to-buffer :around 'dwm-switch-to-buffer))
    (advice-remove 'delete-window 'dwm-continue-main-window)
    (advice-remove 'switch-to-buffer-other-window 'dwm-switch-to-buffer)
    (advice-remove 'switch-to-buffer 'dwm-switch-to-buffer)
    (advice-remove 'pop-to-buffer 'dwm-switch-to-buffer)
    ))

(provide 'dwm)
;;; dwm.el ends here
