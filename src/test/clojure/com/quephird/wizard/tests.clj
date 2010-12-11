(ns com.quephird.wizard.tests
  (:use com.quephird.wizard)
  (:use [clojure.test :only (deftest testing is run-tests use-fixtures)]))

(defn reset-globals [test-fn]
  ((reset! *current-location* :living-room)
   (reset! *object-locations*
     {:whiskey-bottle :living-room,
      :bucket :living-room,
      :chain :garden,
      :frog :garden})
   (test-fn)))

(use-fixtures :each reset-globals)

(deftest testing-describe-location
  (testing "The describe-location function for the current location."
    (is (= (describe-location (deref *current-location*) *nodes*)
           '(You are in the living-room. A wizard is snorting loudly on the couch.))))
)

(deftest testing-describe-paths
  (testing "The describe-paths function for the current location."
    (is (= (describe-paths (deref *current-location*) *edges*)
           '(There is a :door going :west from here. There is a :ladder going :upstairs from here.))))
)

(deftest testing-objects-at
  (testing "The objects-at function for the current location."
    (is (= (objects-at (deref *current-location*) *objects* (deref *object-locations*))
           '(:whiskey-bottle :bucket))))
)

(deftest testing-walk
  (testing "walk-ing from the current location should fail if going :north."
    (is (= (walk :north)
           '(You cannot go that way.))))
  (testing "walk-ing from the current location should succeed if going :west."
    (is (= (do (walk :west) (deref *current-location*))
           :garden)))
)

(deftest testing-pickup
  (testing "Picking up chain from the current location should fail."
    (is (= (pickup :chain)
           '(You cannot get that.))))
  (testing "Picking up whiskey bottle from the current location should succeed."
    (is (= (do (pickup :whiskey-bottle) ((deref *object-locations*) :whiskey-bottle))
           :body)))
)

(run-tests)
