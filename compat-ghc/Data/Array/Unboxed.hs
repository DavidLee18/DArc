-- MicroHs shim: UArray is just a boxed Array (no true unboxed arrays in MicroHs).
module Data.Array.Unboxed (UArray, module Mhs.Array) where
import Mhs.Array

type UArray i e = Array i e
