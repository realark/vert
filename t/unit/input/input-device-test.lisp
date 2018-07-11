(in-package :recurse.vert/unit-test)

(deftest input-device
    (let ((input (make-instance 'input-device :input-name "test-input")))
      (activate-input input :w)
      (activate-input input :d)
      (activate-input input :space)
      (prove:is (get-active-inputs input)
                #(:w :d :space)
                "Three inputs active"
                :test #'equalp)
      (prove:is (get-deactivated-inputs input)
                #()
                "No inputs deactivated"
                :test #'equalp)

      (deactivate-input input :d)
      (prove:is (get-active-inputs input)
                #(:w :space)
                "Two inputs active"
                :test #'equalp)
      (prove:is (get-deactivated-inputs input)
                #(:d)
                "One input deactivated"
                :test #'equalp)

      (recurse.vert::after-input-update input)
      (prove:is (get-active-inputs input)
                #(:w :space)
                "Input update does not affect active inputs."
                :test #'equalp)
      (prove:is (get-deactivated-inputs input)
                #()
                "Input update clears deactivated"
                :test #'equalp)

      (activate-input input :a)
      (activate-input input :a)
      (prove:is (get-active-inputs input)
                #(:w :space :a)
                "Duplicate input ignored."
                :test #'equalp)
      (deactivate-input input :space)
      (deactivate-input input :space)
      (prove:is (get-active-inputs input)
                #(:w :a)
                "Duplicate deactivate ignored."
                :test #'equalp)
      (prove:is (get-deactivated-inputs input)
                #(:space)
                "Duplicate deactivate ignored."
                :test #'equalp)))
