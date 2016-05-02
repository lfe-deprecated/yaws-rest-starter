(defmodule yrests-tests
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest noop
  (is-equal 1 1))
