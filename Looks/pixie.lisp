(in-package :CLIM-INTERNALS)

;;;
;
; This file is in transition, please don't fix it :]
;
; Much is left purposefully un-refactored until enough is reworked that it can be
; refactored into sensible subcomponents, without becoming a hodge-podge of random
; mixins (as opposed to a hodge-podge of sensible mixins :]) - BTS
;
; This especially applies to colour and event management :\
;
;;;

(export '(pixie-look pixie/clx-look))

(defclass pixie-look (frame-manager) ())
(defclass pixie/clx-look (pixie-look clim-clx::clx-frame-manager) ())

; our stub inside clim proper
(defmethod make-pane-1 ((fm pixie-look) (frame application-frame) type &rest args)
  (apply #'make-instance
         (or (find-symbol (concatenate 'string "PIXIE-" (symbol-name type)) :climi)
             (find-symbol (concatenate 'string "PIXIE-" (symbol-name type) "-PANE") :climi)
             ; drop back to the built ins...
             (find-symbol (concatenate 'string (symbol-name type) "-PANE") :climi)
             type)
         :frame frame
         :manager fm
         :port (port frame)
         args))

; Standard

; TODO - clean up all of this colour nonsense
; which should involve some sensible ideas about tints vs' inks

(defclass pixie-gadget () (
   (highlighted      :initarg :highlight
                     :initform +gray93+
                     :reader pane-foreground)
   (paper-color      :initarg :paper-color
                     :initform +white+
                     :reader pane-paper-color)
   (inking-color     :initarg :inking-color
                     :initform +black+
                     :reader pane-inking-color)
   (foreground       :initarg :foreground
                     :initform +gray83+
                     :reader pane-foreground)
   (background       :initarg :background
                     :initform +gray76+
                     :reader pane-background)))

; Highlighting (could the defaults be less horrible?)

(defmethod gadget-highlight-background ((gadget pixie-gadget))
  +gray93+)

(defmethod effective-gadget-foreground ((gadget pixie-gadget))
  (if (slot-value gadget 'armed)
      +gray93+
      +gray83+))

(defmethod effective-gadget-background ((gadget pixie-gadget))
  (if (slot-value gadget 'armed)
      (gadget-highlight-background gadget)
      (pane-background gadget)))

(defmethod effective-gadget-input-area-color ((gadget pixie-gadget))
  +white+)

; refactor this into some kind of dragging mixin
(defclass enter/exit-arms/disarms-unless-dragging-mixin () ())

(defmethod handle-event :before ((pane enter/exit-arms/disarms-unless-dragging-mixin) (event pointer-enter-event))
  (declare (ignorable event))
  (with-slots (dragging) pane
    (unless dragging
      (arm-gadget pane))))

(defmethod handle-event :after ((pane enter/exit-arms/disarms-unless-dragging-mixin) (event pointer-exit-event))
  (declare (ignorable event))
  (with-slots (dragging) pane
    (unless dragging
      (disarm-gadget pane))))

; Slider (refactor into 'thumbed' gadget?)

(defconstant +pixie-slider-pane-thumb-size+  5000.0)
(defconstant +pixie-slider-thumb-half-height+ 17)
(defconstant +pixie-slider-thumb-height+      34)
(defconstant +pixie-slider-thumb-half-width+   8)

(defclass pixie-slider-pane (pixie-gadget slider-pane arm/disarm-repaint-mixin enter/exit-arms/disarms-mixin)
  ((dragging
    :initform nil)
   (drag-delta
     :initform 0)
   (bounce-value
     :initform 0))
  (:default-initargs
    :border-style :inset
    :border-width 1))

(defmethod vertical-gadget-orientation-transformation ((pane gadget))
  (ecase (gadget-orientation pane)
    (:vertical   +identity-transformation+)
    (:horizontal (make-transformation 0 1 1 0 0 0))))

(defun translate-range-value (a mina maxa mino maxo)
  "When \arg{a} is some value in the range from \arg{mina} to \arg{maxa},
   proportionally translate the value into the range \arg{mino} to \arg{maxo}."
  (+ mino (* (/ (- a mina) (- maxa mina)) (- maxo mino))))

(defmethod gadget-thumb-region ((pane pixie-slider-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (gadget-bed-region pane)
    (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 1) (+ y1 1) (- x2 1) (- y2 1))
      (multiple-value-bind (minv maxv) (gadget-range* pane)
        (multiple-value-bind (v) (gadget-value pane)
          (let ((thumb-size (/ +pixie-slider-pane-thumb-size+ (- y2 y1))))
            (let ((ya (translate-range-value v minv (+ maxv thumb-size) y1 y2))
                  (yb (translate-range-value (+ v thumb-size) minv
                                             (+ maxv thumb-size) y1 y2)))
              (make-rectangle* x1 ya x2 yb))))))))

(defmethod gadget-bed-region ((pane pixie-slider-pane))
  (with-bounding-rectangle* (minx miny maxx maxy)
      (transform-region (vertical-gadget-orientation-transformation pane)
                        (sheet-region pane))
    (let* ((middle     (/ (+ maxx minx) 2))
           (minx       (- middle +pixie-slider-thumb-half-width+))
           (maxx       (+ middle +pixie-slider-thumb-half-width+)))
      (make-rectangle* (+ minx 1) (+ miny 1)
                       (- maxx 1) (- maxy 1)))))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-enter-event))
  (with-slots (armed dragging) pane
    (cond
     ((not armed)
      (setf armed t)
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
     (dragging
      (setf dragging :inside)))))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-exit-event))
  (with-slots (armed dragging) pane
    (cond
     (dragging
      (setf dragging :outside))
     (armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane))))))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-button-release-event))
  (with-slots (armed dragging value bounce-value) pane
    (when dragging
      (unless (eq dragging :inside)
        (setf armed nil
            ; value bounce-value ; this bouncing is more annoying than anything for sliders
            )
        (disarmed-callback pane (gadget-client pane) (gadget-id pane)))
      (setf dragging nil)
      (dispatch-repaint pane (sheet-region pane)))))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-button-press-event))
  (multiple-value-bind (x y) (transform-position (vertical-gadget-orientation-transformation pane)
                                                 (pointer-event-x event) (pointer-event-y event))
    (with-slots (armed dragging drag-delta value bounce-value) pane
      (cond
        ((region-contains-position-p (gadget-thumb-region pane) x y)
         ; Thumb
         (setf dragging     :inside
               armed        t
               bounce-value value
               drag-delta   (- y (bounding-rectangle-min-y (gadget-thumb-region pane)))))
        (t
         ; We only care about the thumb
         nil)))))

(defmethod handle-event ((pane pixie-slider-pane) (event pointer-motion-event))
  (with-slots (dragging drag-delta) pane
    (multiple-value-bind (x y)
        (transform-position (vertical-gadget-orientation-transformation pane)
                            (pointer-event-x event) (pointer-event-y event))
      (when dragging
        (let* ((y-new-thumb-top (- y drag-delta))
               (bed-region      (gadget-bed-region pane))
               (miny            (bounding-rectangle-min-y bed-region))
               (maxy            (bounding-rectangle-max-y bed-region))
               (thumb-size      (/ +pixie-slider-pane-thumb-size+ (- maxy miny)))
               (value (min (gadget-max-value pane)
                           (max (gadget-min-value pane)
                                (translate-range-value y-new-thumb-top
                                              (bounding-rectangle-min-y (gadget-bed-region pane))
                                              (bounding-rectangle-max-y (gadget-bed-region pane))
                                              (gadget-min-value pane)
                                              (+ (gadget-max-value pane) thumb-size))))))
	  (setf (gadget-value pane :invoke-callback nil) value)
	  (drag-callback pane (gadget-client pane) (gadget-id pane) value)
	  (dispatch-repaint pane (sheet-region pane)))))))

;;; Repaint

(defmethod handle-repaint ((pane pixie-slider-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let ((tr (vertical-gadget-orientation-transformation pane)))
      (with-bounding-rectangle* (minx miny maxx maxy) (transform-region tr (sheet-region pane))
        (with-drawing-options (pane :transformation tr)
          (with-drawing-options (pane :clipping-region  (transform-region tr
                                                          (region-difference
                                                            (sheet-region pane)
                                                            (gadget-bed-region pane))))
            (draw-rectangle* pane minx miny maxx maxy :filled t :ink *3d-normal-color*))
          ;; draw bed
          (with-bounding-rectangle* (x1 y1 x2 y2) (gadget-bed-region pane)
            (with-drawing-options (pane :clipping-region  (transform-region tr
                                                            (region-difference
                                                              (sheet-region pane)
                                                              (gadget-thumb-region pane))))
              (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 1) (+ y1 1)
                                                         (- x2 1) (- y2 1))
                (draw-rectangle* pane x1 y1 x2 y2 :ink (pane-background pane)))
              (draw-bordered-polygon pane
                                     (polygon-points (make-rectangle* x1 y1 x2 y2))
                                     :style :inset
                                     :border-width 1)))
          ;; draw thumb
          (with-bounding-rectangle* (x1 y1 x2 y2) (gadget-thumb-region pane)
            (let ((x2 (- x2 1)))
              (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-foreground pane))
              ;; white outline
              (draw-line* pane x1 y2 x1 y1 :ink +white+)
              (draw-line* pane x2 y1 x1 y1 :ink +white+)
              ;; now for the gray inline
              (let ((x1 (+ x1 2))
                    (y1 (+ y1 2))
                    (x2 (- x2 1))
                    (y2 (- y2 1)))
                (draw-line* pane x1 y2 (+ x2 1) y2 :ink +gray58+)
                (draw-line* pane x2 y1 x2 (+ y2 1) :ink +gray58+))
              ;; now for the black outline
              (draw-line* pane x1 y2 (+ x2 1) y2 :ink +black+)
              (draw-line* pane x2 y1 x2 (+ y2 1) :ink +black+))
            ;; draw decoration in the thumb
            (let* ((middle (/ (+ y1 y2) 2))
                   (y1     (- middle 1))
                   (y2     middle)
                   (x1     (+ x1 2))
                   (x2     (- x2 3)))
              (draw-line* pane x1 y1 x2 y1       :ink +gray58+)
              (draw-line* pane x1 y2 (+ x2 1) y2 :ink +white+)
              (draw-line* pane x2 y1 x2 (+ y2 1) :ink +white+))))))))

; Scrollbar

; We derive from the slider, since the slider is the same, only
; less so.

(defconstant +pixie-scroll-bar-pane-thumb-size+  5000.0)
(defconstant +pixie-scroll-bar-thumb-half-height+ 17)
(defconstant +pixie-scroll-bar-thumb-height+      34)
(defconstant +pixie-scroll-bar-thumb-half-width+   8)

(defclass pixie-scroll-bar-pane (pixie-slider-pane) (
   (drag-callback :initarg :drag-callback
		  :initform nil
		  :reader scroll-bar-drag-callback)
   (scroll-to-bottom-callback :initarg :scroll-to-bottom-callback
			      :initform nil
			      :reader scroll-bar-scroll-to-bottom-callback)
   (scroll-to-top-callback :initarg :scroll-to-top-callback
			   :initform nil
			   :reader scroll-bar-scroll-to-top-callback)
   (scroll-down-line-callback :initarg :scroll-down-line-callback
                              :initform nil
                              :reader scroll-bar-scroll-down-line-callback)
   (scroll-up-line-callback :initarg :scroll-up-line-callback
                            :initform nil
                            :reader scroll-bar-scroll-up-line-callback)
   (scroll-down-page-callback :initarg :scroll-down-page-callback
                              :initform nil
                              :reader scroll-bar-scroll-down-page-callback)
   (scroll-up-page-callback :initarg :scroll-up-page-callback
                            :initform nil
                            :reader scroll-bar-scroll-up-page-callback)
   (thumb-size :initarg :thumb-size :initform 1/4
               :accessor gadget-thumb-size)
   )
  (:default-initargs :value 0
                     :min-value 0
                     :max-value 1
                     :orientation :vertical))

(defmethod drag-callback ((pane pixie-scroll-bar-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-drag-callback pane) value))

(defmethod scroll-to-top-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-to-top-callback pane)))

(defmethod scroll-to-bottom-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-to-bottom-callback pane)))

(defmethod scroll-up-line-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-up-line-callback pane)))

(defmethod scroll-up-page-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-up-page-callback pane)))

(defmethod scroll-down-line-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-down-line-callback pane)))

(defmethod scroll-down-page-callback ((pane pixie-scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-down-page-callback pane)))


(defmethod scroll-bar-thumb-size ((pane pixie-scroll-bar-pane))
  (gadget-thumb-size pane))

(defmethod (setf scroll-bar-thumb-size) (value (pane pixie-scroll-bar-pane))
  (setf (gadget-thumb-size pane) value))

(defmethod gadget-up-region ((pane pixie-scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx ma)
      (transform-region (vertical-gadget-orientation-transformation pane)
                        (sheet-region pane))
    (declare (ignore maxy))
    (make-rectangle* minx miny
                     maxx (+ miny (- maxx minx)))))

(defmethod gadget-down-region ((pane pixie-scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy)
      (transform-region (vertical-gadget-orientation-transformation pane)
                        (sheet-region pane))
    (declare (ignore miny))
    (make-rectangle* minx (- maxy (- maxx minx))
                     maxx maxy)))

(defmethod gadget-bed-region ((pane pixie-scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy)
      (transform-region (vertical-gadget-orientation-transformation pane)
                        (sheet-region pane))
    (make-rectangle* minx (+ miny (- maxx minx) 1)
                     maxx (- maxy (- maxx minx) 1))))

(defmethod gadget-thumb-region ((pane pixie-scroll-bar-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (gadget-bed-region pane)
    (multiple-value-bind (minv maxv) (gadget-range* pane)
      (multiple-value-bind (v) (gadget-value pane)
        (let ((ts (gadget-thumb-size pane)))
          (let ((ya (translate-range-value v minv (+ maxv ts) y1 y2))
                (yb (translate-range-value (+ v ts) minv (+ maxv ts) y1 y2)))
            (make-rectangle* x1 ya x2 yb)))))))

(defmethod handle-event ((pane pixie-scroll-bar-pane) (event pointer-enter-event))
  (with-slots (armed dragging) pane
    (cond
     ((not armed)
      (setf armed t)
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
     (dragging
      (setf dragging :inside)))))

(defmethod handle-event ((pane pixie-scroll-bar-pane) (event pointer-exit-event))
  (with-slots (armed dragging) pane
    (cond
     (dragging
      (setf dragging :outside))
     (armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane))))))

(defmethod handle-event ((pane pixie-scroll-bar-pane) (event pointer-motion-event))
  (with-slots (dragging) pane
    (when dragging
      (let ((value (convert-position-to-value
                     pane
                     (if (eq (gadget-orientation pane) :vertical)
                             (pointer-event-y event)
                             (pointer-event-x event)))))
	(setf (gadget-value pane :invoke-callback nil) value)
	(drag-callback pane (gadget-client pane) (gadget-id pane) value)
	(dispatch-repaint pane (sheet-region pane))))))

(defmethod handle-event ((pane pixie-scroll-bar-pane) (event pointer-button-release-event))
  (with-slots (armed dragging) pane
    (when dragging
      (unless (eq dragging :inside)
        (setf armed nil)
        (disarmed-callback pane (gadget-client pane) (gadget-id pane)))
      (setf dragging nil)
      (dispatch-repaint pane (sheet-region pane)))))

(defmethod handle-event ((pane pixie-scroll-bar-pane) (event pointer-button-press-event))
  (multiple-value-bind (x y) (transform-position (vertical-gadget-orientation-transformation pane)
                                                 (pointer-event-x event) (pointer-event-y event))
    (with-slots (armed dragging drag-delta) pane
      (cond
        ((region-contains-position-p (gadget-up-region pane) x y)
         ; Up Arrow
         (scroll-up-line-callback pane (gadget-client pane) (gadget-id pane))
         (setf (slot-value pane 'armed) :up)
         (dispatch-repaint pane +everywhere+))
        ((region-contains-position-p (gadget-down-region pane) x y)
         ; Down Arrow
         (scroll-down-line-callback pane (gadget-client pane) (gadget-id pane))
         (setf (slot-value pane 'armed) :down)
         (dispatch-repaint pane +everywhere+))
        ((region-contains-position-p (gadget-thumb-region pane) x y)
         ; Thumb
         (setf dragging :inside
               armed    t
               drag-delta (- y (bounding-rectangle-min-y (gadget-thumb-region pane)))))
        ((region-contains-position-p (gadget-bed-region pane) x y)
         ; Bed
         (if (< y (bounding-rectangle-min-y (gadget-bed-region pane)))
             (scroll-up-page-callback pane (gadget-client pane) (gadget-id pane))
             (scroll-down-page-callback pane (gadget-client pane) (gadget-id pane))))
        (t
         ; Nowhere (!)
         nil)))))

(defmethod handle-event ((pane pixie-scroll-bar-pane) (event pointer-motion-event))
  (with-slots (dragging drag-delta) pane
    (multiple-value-bind (x y)
        (transform-position (vertical-gadget-orientation-transformation pane)
                            (pointer-event-x event) (pointer-event-y event))
      (when dragging
        (let* ((y-new-thumb-top (- y drag-delta))
               (ts (gadget-thumb-size pane))
               (value (min (gadget-max-value pane)
                           (max (gadget-min-value pane)
                                (translate-range-value y-new-thumb-top
                                              (bounding-rectangle-min-y (gadget-bed-region pane))
                                              (bounding-rectangle-max-y (gadget-bed-region pane))
                                              (gadget-min-value pane)
                                              (+ (gadget-max-value pane) ts))))))
	  (setf (gadget-value pane :invoke-callback nil) value)
	  (drag-callback pane (gadget-client pane) (gadget-id pane) value)
	  (dispatch-repaint pane (sheet-region pane)))))))

;;; Repaint

(defmethod handle-repaint ((pane pixie-scroll-bar-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let ((tr (vertical-gadget-orientation-transformation pane)))
      (with-bounding-rectangle* (minx miny maxx maxy) (transform-region tr (sheet-region pane))
        (with-drawing-options (pane :transformation tr)
          (draw-rectangle* pane minx miny maxx maxy :filled t
                           :ink *3d-normal-color*)
          ;; draw up arrow
          (with-bounding-rectangle* (x1 y1 x2 y2) (gadget-up-region pane)
            (let ((pg (list (make-point (/ (+ x1 x2) 2) y1)
                            (make-point x1 y2)
                            (make-point x2 y2))))
              (case (slot-value pane 'armed)
                (:up
                 (draw-polygon pane pg :ink *3d-inner-color*)
                 (draw-bordered-polygon pane pg :style :inset :border-width 2))
                (otherwise
                 (draw-polygon pane pg :ink *3d-normal-color*)
                 (draw-bordered-polygon pane pg :style :outset :border-width 2) ))))

          ;; draw down arrow
          (with-bounding-rectangle* (x1 y1 x2 y2) (gadget-down-region pane)
            (let ((pg (list (make-point (/ (+ x1 x2) 2) y2)
                            (make-point x1 y1)
                            (make-point x2 y1))))
              (case (slot-value pane 'armed)
                (:down
                 (draw-polygon pane pg :ink *3d-inner-color*)
                 (draw-bordered-polygon pane pg :style :inset :border-width 2))
                (otherwise
                 (draw-polygon pane pg :ink *3d-normal-color*)
                 (draw-bordered-polygon pane pg :style :outset :border-width 2)))))

          ;; draw thumb
          (with-bounding-rectangle* (x1 y1 x2 y2) (gadget-thumb-region pane)
            (draw-rectangle* pane x1 y1 x2 y2 :ink *3d-normal-color*)
            (draw-bordered-polygon pane
                                   (polygon-points (make-rectangle* x1 y1 x2 y2))
                                   :style :outset
                                   :border-width 2)
            (let ((y (/ (+ y1 y2) 2)))
              (draw-bordered-polygon pane
                                     (polygon-points (make-rectangle* (+ x1 3) (- y 1) (- x2 3) (+ y 1)))
                                     :style :inset
                                     :border-width 1)
              (draw-bordered-polygon pane
                                     (polygon-points (make-rectangle* (+ x1 3) (- y 4) (- x2 3) (- y 2)))
                                     :style :inset
                                     :border-width 1)
              (draw-bordered-polygon pane
                                     (polygon-points (make-rectangle* (+ x1 3) (+ y 4) (- x2 3) (+ y 2)))
                                     :style :inset
                                     :border-width 1))) )))))

; Menus

(defclass pixie-menu-bar-pane (pixie-gadget menu-bar) ())

(defmethod handle-repaint ((pane pixie-menu-bar-pane) region)
  (with-special-choices (pane)
    (let* ((region (sheet-region pane))
           (frame (polygon-points (bounding-rectangle region))))
      (draw-polygon pane frame :ink +Blue+ :filled t)
      (draw-bordered-polygon pane frame :style :outset :border-width 1))))

(defclass pixie-menu-button-pane (pixie-gadget menu-button-pane) ())

(defmethod handle-event ((pane menu-button-pane) (event pointer-enter-event))
  (when (slot-value (slot-value pane 'client) 'armed)
    (arm-branch pane)))

(defmethod handle-event ((pane menu-button-pane) (event pointer-button-press-event))
  (arm-branch pane))

(defmethod handle-event ((pane menu-button-pane) (event pointer-ungrab-event))
  (destroy-substructure (menu-root pane)))


(defmethod handle-repaint ((pane pixie-menu-button-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let* ((region (sheet-region pane))
           (frame (polygon-points (bounding-rectangle region))))
      (draw-polygon pane frame :filled t :ink (effective-gadget-foreground pane))
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
        (cond
          ((slot-value pane 'armed)
           (let ((inset-frame (polygon-points
                                (make-rectangle* (+ x1 2) (+ y1 2) (- x2 2) (- y2 2)))))
             (draw-polygon pane inset-frame :filled t :ink (effective-gadget-foreground pane))
             (draw-bordered-polygon pane inset-frame
                           :style :outset
                           :border-width 1)))
          (t
           (draw-polygon pane frame :filled t :ink (effective-gadget-foreground pane))))
        (draw-label* pane x1 y1 x2 y2 :ink (pane-inking-color pane))))))

(defmethod compose-space ((gadget pixie-menu-button-pane) &key width height)
  (declare (ignore width height))
  (space-requirement+* (space-requirement+* (compose-label-space gadget :wider 15 :higher 10)
                                            :min-width (* 2 (pane-x-spacing gadget))
                                            :width (* 2 (pane-x-spacing gadget))
                                            :max-width (* 2 (pane-x-spacing gadget))
                                            :min-height (* 2 (pane-y-spacing gadget))
                                            :height (* 2 (pane-y-spacing gadget))
                                            :max-height (* 2 (pane-y-spacing gadget)))
                       :min-width (* 2 *3d-border-thickness*)
                       :width (* 2 *3d-border-thickness*)
                       :max-width (* 2 *3d-border-thickness*)
                       :min-height (* 2 *3d-border-thickness*)
                       :height (* 2 *3d-border-thickness*)
                       :max-height (* 2 *3d-border-thickness*)))

(defclass pixie-menu-button-leaf-pane (pixie-menu-button-pane menu-button-leaf-pane) ())

(defclass pixie-menu-button-submenu-pane (pixie-menu-button-pane menu-button-submenu-pane) ())

(defmethod repaint-sheet ((pane pixie-menu-button-submenu-pane) region)
  (declare (ignore region))
  (with-slots (submenu-frame) pane
    (if submenu-frame
	(menu-draw-highlighted pane)
	(menu-draw-unhighlighted pane))))

; Image pane

; rebuild this with a pixmap repository for general re-use of pixmaps
; within a port/visual combination.

; This is just test/proof-of-concept code :]

(defclass pixie-image-pane (pixie-gadget basic-gadget) (
  (image-pathname :initarg :pathname)
  (image-mask-pathname :initarg :mask-pathname :initform nil)
  (image-width    :type integer
                  :reader width
                  :initform 0)
  (image-height   :type integer
                  :reader height
                  :initform 0)
  (image-image    :initform nil)
  (image-pixmap   :initform nil)
  (image-stencil  :initform nil)))

; TODO: allow pixmaps to be realized from unrealized media
(defmethod initialize-instance :after ((pane pixie-image-pane) &rest args)
  (declare (ignore args))
  (with-slots (image-pathname image-image image-width image-height) pane
    (let* ((data (image:read-image-file image-pathname))
           (image (image:make-truecolor-image data 255)))
      (destructuring-bind (width height) (array-dimensions data)
        (setf image-width  width
              image-height height
              image-image  image))))
  (with-slots (image-mask-pathname image-stencil) pane
    (when image-mask-pathname
      (let* ((data (image:read-image-file image-mask-pathname)))
        (setf image-stencil (make-stencil data))))))

(defmethod handle-repaint ((pane pixie-image-pane) region)
  (declare (ignore region))
  (with-slots (image-pixmap image-width image-height) pane
    ; we defer the image loading until after realisation
    ; which will cause a delay in the initial exposure,
    ; CHECKME - should we be able to realize pixmaps derived
    ; from unrealized panes?
    ; Technically we could just do it from the port's root
    ; since we don't switch visuals within a port at this point.
    ; but that is not necessarily a good thing
    (unless image-pixmap
      (with-slots (image-image image-width image-height image-pixmap) pane
        (setf image-pixmap
              (with-output-to-pixmap (medium pane :width image-width :height image-height)
                (draw-image (medium-sheet medium)
                            image-image
                            :clipping-region (make-rectangle* 0 0 image-width image-height))))))
    (copy-from-pixmap image-pixmap 0 0 image-width image-height pane 0 0)))

(defmethod compose-space ((pane pixie-image-pane) &key width height)
  (declare (ignore width height))
  (with-slots (image-width image-height) pane
    (let ((w image-width)
          (h image-height))
      (make-space-requirement :width     w :height     h
                              :min-width w :min-height h
                              :max-width w :max-height h))))

; Toggle Button (for checkboxes and radio-buttons)

(defclass pixie-toggle-button-pane (pixie-gadget toggle-button-pane) ())

(defmethod draw-toggle-button-indicator ((pane pixie-toggle-button-pane) (type (eql :one-of)) value x1 y1 x2 y2)
  (multiple-value-bind (cx cy) (values (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
    (let ((radius (/ (- y2 y1) 2)))
      (draw-circle* pane cx cy radius
                     :start-angle (* 1/4 pi)
                     :end-angle (* 5/4 pi)
                     :ink *3d-dark-color*)
      (draw-circle* pane cx cy (- radius 1)
                     :start-angle (* 1/4 pi)
                     :end-angle (* 5/4 pi)
                     :ink (pane-inking-color pane))
      (draw-circle* pane cx cy radius
                     :start-angle (* 5/4 pi)
                     :end-angle (* 9/4 pi)
                     :ink *3d-light-color*)
      (draw-circle* pane cx cy (- radius 1)
                     :start-angle (* 5/4 pi)
                     :end-angle (* 9/4 pi)
                     :ink (effective-gadget-foreground pane))
      (draw-circle* pane cx cy (max 1 (- radius 2))
                     :ink (pane-paper-color pane))
      (when value
        (draw-circle* pane cx cy (max 1 (- radius 4))
                       :ink (pane-inking-color pane))))))

(defmethod draw-toggle-button-indicator ((pane pixie-toggle-button-pane) (type (eql :some-of)) value x1 y1 x2 y2)
  (draw-rectangle* pane x1 y1 x2 y2 :ink (pane-paper-color pane))
  (draw-bordered-rectangle* pane x1 y1 x2 y2 :style :inset)
  (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 1) (+ y1 1)
                                             (- x2 2) (- y2 2))
    (draw-line* pane x1 y2 x2 y2 :ink (effective-gadget-foreground pane))
    (draw-line* pane x2 y1 x2 y2 :ink (effective-gadget-foreground pane))
    (draw-line* pane x1 y1 x1 (+ y2 1) :ink (pane-inking-color pane))
    (draw-line* pane x1 y1 (+ x2 1) y1 :ink (pane-inking-color pane)))
  (when value
    (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 3) (+ y1 3)
                                               (- x2 3) (- y2 3))
      (draw-line* pane x1 y1 x2 y2 :ink (pane-inking-color pane) :line-thickness 2)
      (draw-line* pane x2 y1 x1 y2 :ink (pane-inking-color pane) :line-thickness 2))))

(defmethod handle-repaint ((pane pixie-toggle-button-pane) region)
  (declare (ignore region))
  (when (sheet-grafted-p pane)
    (with-special-choices (pane)
      (with-slots (armed) pane
        (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
          (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-background pane))
          (let* ((as (text-style-ascent (pane-text-style pane) pane))
                 (ds (text-style-descent (pane-text-style pane) pane)) )
            (multiple-value-bind (tx1 ty1 tx2 ty2)
                (values (+ x1 (pane-x-spacing pane))
                        (- (/ (+ y1 y2) 2) (/ (+ as ds) 2))
                        (+ x1 (pane-x-spacing pane) (+ as ds))
                        (+ (/ (+ y1 y2) 2) (/ (+ as ds) 2)))
              (draw-toggle-button-indicator pane (toggle-button-indicator-type pane) (gadget-value pane)
                                            tx1 ty1 tx2 ty2)
              (draw-label* pane (+ tx2 (pane-x-spacing pane)) y1 x2 y2
                           :ink (pane-inking-color pane)))))))))

; Push Button

(defclass pixie-push-button-pane  (pixie-gadget push-button-pane slider-pane) (
  (dragging
    :initform nil)))

; factor out the dragging code into a mixin
(defmethod handle-event ((pane pixie-push-button-pane) (event pointer-enter-event))
  (with-slots (armed dragging) pane
    (cond
     ((not armed)
(print "arming")
      (setf armed t)
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
     (dragging
      (setf dragging :inside))))
  (dispatch-repaint pane +everywhere+))

(defmethod handle-event ((pane pixie-push-button-pane) (event pointer-exit-event))
  (with-slots (armed dragging) pane
    (cond
     (dragging
      (setf dragging :outside))
     (armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))
  (dispatch-repaint pane +everywhere+))

(defmethod handle-event ((pane pixie-push-button-pane) (event pointer-button-press-event))
  (with-slots (pressedp dragging) pane
    (setf pressedp t
          dragging :inside)
    (dispatch-repaint pane +everywhere+)))

(defmethod handle-event ((pane pixie-push-button-pane) (event pointer-button-release-event))
  (with-slots (armed pressedp dragging) pane
    (setf pressedp nil)
    (when (and armed (eq dragging :inside))
      (activate-callback pane (gadget-client pane) (gadget-id pane))
      (setf pressedp nil)
      (dispatch-repaint pane +everywhere+))))

; factor out the box code and reuse as a border mixin
(defmethod handle-repaint ((pane pixie-push-button-pane) region)
  (declare (ignore region))
  (with-slots (armed dragging pressedp) pane
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (let ((x1 (+ x1 1))
            (y1 (+ y1 1))
            (x2 (- x2 1))
            (y2 (- y2 1)))
        (let ((x2 (- x2 1))
              (y2 (- y2 1)))
          (cond
           ((or (not pressedp)
                (eq dragging :outside))
            (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-foreground pane))
            ;; white outline
            (draw-line* pane x1 y2 x1 y1 :ink +white+)
            (draw-line* pane x2 y1 x1 y1 :ink +white+)
            ;; now for the gray inline
            (let ((x1 (+ x1 2))
                  (y1 (+ y1 2))
                  (x2 (- x2 1))
                  (y2 (- y2 1)))
              (draw-line* pane x1 y2 (+ x2 1) y2 :ink +gray54+) ; <- not a typo
              (draw-line* pane x2 y1 x2 (+ y2 1) :ink +gray54+))
            ;; now for the black outline
            (draw-line* pane x1 y2 (+ x2 1) y2 :ink +black+)
            (draw-line* pane x2 y1 x2 (+ y2 1) :ink +black+)
            (draw-label* pane x1 y1 x2 y2
                       :ink (pane-inking-color pane)))
           (pressedp
            (draw-rectangle* pane x1 y1 x2 y2 :ink (pane-background pane))
            ;; white outline
            (draw-line* pane x1 y2 x1 y1 :ink +gray58+)
            (draw-line* pane x2 y1 x1 y1 :ink +gray58+)
            ;; now for the black inline
            (let ((x1 (+ x1 1))
                  (y1 (+ y1 1))
                  (x2 (- x2 2))
                  (y2 (- y2 2)))
              (draw-line* pane x1 y1 (+ x2 1) y1 :ink +black+)
              (draw-line* pane x1 y1 x1 (+ y2 1) :ink +black+))
            ;; now for the black outline
            (draw-line* pane x1 y2 (+ x2 1) y2 :ink +white+)
            (draw-line* pane x2 y1 x2 (+ y2 1) :ink +white+)
            (draw-label* pane x1 y1 x2 y2
                       :ink (pane-inking-color pane)))))))))