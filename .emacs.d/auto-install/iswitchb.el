;;; iswitchb.el --- switch between buffers using substrings

;; Copyright (C) 1996, 1997, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Stephen Eglen <stephen@gnu.org>
;; Maintainer: Stephen Eglen <stephen@gnu.org>
;; Keywords: completion convenience

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Installation:
;; To get the functions in this package bound to keys, use
;; M-x iswitchb-mode or customize the option `iswitchb-mode'.
;; Alternatively, add the following line to your .emacs:
;; (iswitchb-mode 1)

;; As you type in a substring, the list of buffers currently matching
;; the substring is displayed as you type.  The list is ordered so
;; that the most recent buffers visited come at the start of the list.
;; The buffer at the start of the list will be the one visited when
;; you press return.  By typing more of the substring, the list is
;; narrowed down so that gradually the buffer you want will be at the
;; top of the list.  Alternatively, you can use C-s and C-r to rotate
;; buffer names in the list until the one you want is at the top of
;; the list.  Completion is also available so that you can see what is
;; common to all of the matching buffers as you type.

;; This code is similar to a couple of other packages.  Michael R Cook
;; <cook@sightpath.com> wrote a similar buffer switching package, but
;; does exact matching rather than substring matching on buffer names.
;; I also modified a couple of functions from icomplete.el to provide
;; the completion feedback in the minibuffer.

;;; Example

;; If I have two buffers called "123456" and "123", with "123456" the
;; most recent, when I use iswitchb, I first of all get presented with
;; the list of all the buffers
;;
;;       iswitch  {123456,123}
;;
;; If I then press 2:
;;       iswitch 2[3]{123456,123}
;;
;; The list in {} are the matching buffers, most recent first (buffers
;; visible in the current frame are put at the end of the list by
;; default).  At any time I can select the item at the head of the
;; list by pressing RET.  I can also put the first element at the end
;; of the list by pressing C-s, or put the last element at the head of
;; the list by pressing C-r.  The item in [] indicates what can be
;; added to my input by pressing TAB.  In this case, I will get "3"
;; added to my input.  So, press TAB:
;; iswitch 23{123456,123}
;;
;; At this point, I still have two matching buffers.
;; If I want the first buffer in the list, I simply press RET.  If I
;; wanted the second in the list, I could press C-s to move it to the
;; top of the list and then RET to select it.
;;
;; However, if I type 4, I only have one match left:
;;       iswitch 234[123456] [Matched]
;;
;; Since there is only one matching buffer left, it is given in [] and we
;; see the text [Matched] afterwards.  I can now press TAB or RET to go
;; to that buffer.
;;
;; If however, I now type "a":
;;       iswitch 234a [No match]
;; There are no matching buffers.  If I press RET or TAB, I can be
;; prompted to create a new buffer called "234a".
;;
;; Of course, where this function comes in really useful is when you
;; can specify the buffer using only a few keystrokes.  In the above
;; example, the quickest way to get to the "123456" buffer would be
;; just to type 4 and then RET (assuming there isn't any newer buffer
;; with 4 in its name).

;; To see a full list of all matching buffers in a sep
