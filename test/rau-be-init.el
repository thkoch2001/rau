(let ((appender
         (lgr-set-layout
          (lgr-appender-journald)
          (lgr-layout-format
           :format "%m"))))
    (let ((lgr (lgr-get-logger "ewc")))
      (lgr-add-appender lgr appender)
      (lgr-set-threshold lgr lgr-level-info))
    (let ((lgr (lgr-get-logger "rau")))
      (lgr-add-appender lgr appender)
      (lgr-set-threshold lgr lgr-level-info)
      ))
