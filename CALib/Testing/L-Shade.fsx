#load "..\Probability.fs"
open Probability

let prob_size = 30
let max_nfes = 10000 * prob_size
let rnd = XorshiftPRNG()
let val_2_reach = 10e-8
let max_region = 100.0
let min_region = -100.0
let lu = []