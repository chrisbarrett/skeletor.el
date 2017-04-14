;;; test-helper.el --- Helper functions to test __PROJECT-NAME__  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 __USER-NAME__

;; Author: __USER-NAME__ <__USER-MAIL-ADDRESS__>

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

(declare-function undercover "undercover")

(when (require 'undercover nil t)
  (undercover "__PROJECT-NAME__.el"))

;;; test-helper.el ends here
