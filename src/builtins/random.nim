const
  N = 624
  M = 397
  MATRIX_A = 0x9908B0DF'u32
  UPPER_MASK = 0x80000000'u32
  LOWER_MASK = 0x7FFFFFFF'u32

  MAGIC_CONSTANT = 1812433253'u32
  SEED_DEFAULT = 5489

type
  MTState = object
    mt: array[N, uint32]
    index: int

proc init_mt(seed: uint32): MTState =
  var s: MTState
  if seed>0:
    s.mt[0] = seed
  else:
    # warn here
    s.mt[0] = SEED_DEFAULT
  for i in 1..<N:
    s.mt[i] = MAGIC_CONSTANT * (s.mt[i-1] xor (s.mt[i-1] shr 30)) + uint32(i)
  s.index = N
  return s