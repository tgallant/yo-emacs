;;; eyo.el --- An Emacs library for a modern frontend workflow

;; Copyright (c) 2014 Tim Gallant <me@timgallant.us>

;; Author: Tim Gallant <me@timgallant.us>
;; URL: https://github.com/tgallant/
;; Keywords: yeoman bower grunt yo
;; Created: 03 Dec 2014
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Deps

(require 'ansi-color)

;; ansi filter for process output

(defun* ansi-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert (ansi-color-apply string))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun* locate-file (filename &optional (dir default-directory))
  (let ((has-gruntfile (directory-files dir nil (format "^%s$" filename)))
        (is-root (equal dir "/")))
    (cond
     (has-gruntfile (format "%s/" dir))
     (is-root
      (print (format
              "No Gruntfile found in either %s or any parent directory!"
              default-directory))
      nil)
     ((locate-file filename (expand-file-name ".." dir))))))

(locate-file "bower.json" "/home/tim/projects/tor-calc/app/scripts/")

;; Functions

(defun grunt-cmd (cmd)
  (set-process-filter
   (start-process-shell-command
    "grunt-log"
    "*grunt-log*"
    "grunt ")
   'ansi-filter)
  (switch-to-buffer-other-window "*grunt-server*"))

(defun yo-cmd (cmd)
  (setq default-directory (locate-file "Gruntfile.js"))
  (message default-directory)
  (set-process-filter
   (start-process-shell-command
    "yo-log"
    "*yo-log*"
    (format "yo %s" cmd))
   'ansi-filter)
  (switch-to-buffer-other-window "*yo-log*"))

(defun bower-cmd (cmd)
  (setq default-directory (locate-file "bower.json"))
  (message default-directory)
  (set-process-filter
   (start-process-shell-command
    "bower-log"
    "*bower-log*"
    (format "bower %s" cmd))
   'ansi-filter)
  (switch-to-buffer-other-window "*bower-log*"))

(defun npm-cmd (cmd)
  (setq default-directory (locate-file "package.json"))
  (message default-directory)
  (set-process-filter
   (start-process-shell-command
    "npm-log"
    "*npm-log*"
    (format "npm %s" cmd))
   'ansi-filter)
  (switch-to-buffer-other-window "*npm-log*"))

;; yeoman commands

(defun yo (cmd)
  (interactive "SCommand: ")
  (yo-cmd cmd))

;; bower commands

(defun bower (cmd)
  (interactive "SCommand: ")
  (bower-cmd cmd))

(defun bower-install ()
  (interactive)
  (bower-cmd "install"))

(defun bower-install-save (pkg)
  (interactive "SPackage: ")
  (bower-cmd (format "install %s --save")))

;; npm commands

(defun npm (cmd)
  (interactive "SCommand: ")
  (npm-cmd cmd))

(defun npm-install ()
  (interactive)
  (npm-cmd "install"))

(defun npm-install-save (pkg)
  (interactive "SPackage: ")
  (npm-cmd (format "install %s --save")))

(defun npm-install-g (pkg)
  (interactive "SPackage: ")
  (npm-cmd (format "install -g %s")))

;; grunt commands

(defun grunt (cmd)
  (interactive "SCommand: ")
  (grunt-cmd cmd))

(defun grunt-server ()
  (interactive)
  (grunt-cmd "server"))

;; default keybindings

(global-set-key (kbd "M-g s") 'grunt-server)
(global-set-key (kbd "M-g b i") 'bower-install)
(global-set-key (kbd "M-g b s") 'bower-install-save)
(global-set-key (kbd "M-g n i") 'npm-install)
(global-set-key (kbd "M-g n s") 'npm-install-save)
(global-set-key (kbd "M-g n g") 'npm-install-g)
