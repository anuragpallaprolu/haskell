--Round E APAC 2017
--Problem D
--Sorting Arrays

import Lib
import Data.Time.Clock
import Control.Parallel
import Control.Parallel.Strategies
import System.Environment
import Control.Exception
import Control.DeepSeq
import Data.List
import Data.Maybe
import Data.List.Split

isContiguous :: [Int] -> Bool
isContiguous [] = False
isContiguous x  = (sort x == [(minimum x)..(maximum x)])

--You need to write a function which takes
--an unsorted array as the input and returns contiguous blocks whenever.
--So if input is [1,2,7,5,6,4,3], the output must look like [[1,2],[7],[5,6,4,3]]
--
--My hypothesis is that for P = 2, the size of this two dimensional array is the value of K.

makeContiguous :: [Int] -> [[Int]] -> [Int] -> Int -> Bool -> [[Int]]
makeContiguous (x:xs) soFar runner maxLen blockEnded | xs == [] && blockEnded == False   = (init soFar') ++ [(last soFar') ++ [x]]
			                             | xs == [] && blockEnded == True    = soFar' ++ [[x]]
				                     | otherwise                         = makeContiguous (if blockEnded then (x:xs) else xs) (if blockEnded then soFar ++ [runner] else soFar) (if (blockEnded) then [] else runner ++ [x]) maxLen (if (blockEnded) then False else (if (isContiguous $ runner ++ [x, head xs]) && length runner < maxLen then False else True)) where soFar' = soFar ++ [runner]

--This partitions the array into contiguous sorted subarrays of upper bound size n.
partition n x = map sort $ makeContiguous x [] [] (n-1) False 


--This swaps two elements in an immutable list and creates a new one.
--Should be a part of Lib.
swap :: [[Int]] -> Int -> Int -> [[Int]]
swap x i j = take (i-1) x ++ [x!!(j-1)] ++ take (j-i-1) (drop i x) ++ [x!!(i-1)] ++ take ((length x) - j) (drop j x)

--The brute force approach will basically perform nC2 swaps for each K value and will compare at which (largest K
--below the optimum) K value there is a complete sort. We need to find a method to compute the optimum too. 

makeContiguousO :: [Int] -> [[Int]] -> [Int] -> Bool -> [[Int]]
makeContiguousO (x:xs) soFar runner blockEnded | xs == [] && blockEnded == False   = (init soFar') ++ [(last soFar') ++ [x]]
                                               | xs == [] && blockEnded == True    = soFar' ++ [[x]]
				               | otherwise                         = makeContiguousO (if blockEnded then (x:xs) else xs) (if blockEnded then soFar ++ [runner] else soFar) (if (blockEnded) then [] else runner ++ [x]) (if (blockEnded) then False else (if isContiguous $ runner ++ [x, head xs] then False else True)) where soFar' = soFar ++ [runner]

optima x = maximum $ map length $ makeContiguousO x [] [] False 

--Given an x, compute partition n x where n goes from 1 to optima x. For each n, compute the swaps and compare to (sort x).
--If true return the length $ partition j x where j is the value of n where the match occurs.
--Lib.Choose is used to make swaps. 

index a b = fromJust $ elemIndex a b

swap_pairs n x = choose [1..(length $ Main.partition n x)] 2

--foundSort returns the number of times the scheme sorts for a given K value.

foundSort :: [Int] -> Int -> Int
foundSort x k = sum [if ((concat (swap x' (y!!0) (y!!1))) == sort x) then 1 else 0 | y <- swap_pairs k x] where x' = Main.partition k x

a = "73 358 319 109 321 195 214 96 308 25 23 350 314 147 28 280 30 167 87 86 76 296 329 125 160 105 177 341 22 230 16 353 119 188 291 240 29 344 313 104 211 78 303 309 316 243 10 99 137 179 336 4 244 311 7 161 55 302 20 39 197 169 258 176 216 187 219 59 36 116 93 249 184 144 154 209 340 58 221 62 139 257 113 41 69 275 326 307 180 224 89 115 210 278 266 232 92 131 328 170 217 127 206 255 84 178 267 100 175 8 272 288 52 229 349 263 24 330 31 347 332 198 259 247 85 183 50 88 13 174 120 293 33 354 252 215 168 304 26 241 276 51 43 171 107 34 2 129 117 324 156 298 193 289 134 218 196 155 282 122 136 148 295 126 283 203 21 68 162 205 163 248 44 70 102 149 325 190 226 301 287 237 74 238 32 194 351 72 82 37 111 121 101 146 60 233 269 71 337 165 246 189 9 277 299 97 65 35 207 357 103 220 145 268 202 235 135 186 223 239 64 158 53 56 63 271 254 294 231 138 124 11 212 80 201 352 5 260 142 256 334 335 38 199 40 333 348 118 270 279 57 132 42 327 128 343 185 222 342 79 318 151 75 264 182 108 192 261 133 262 18 95 234 284 66 83 166 306 200 250 153 94 317 251 46 152 292 290 77 274 27 172 286 1 112 98 345 15 346 67 356 17 242 140 331 12 339 315 181 281 312 225 123 236 143 90 54 285 300 355 208 173 14 191 320 164 323 61 19 110 81 338 245 265 204 49 114 227 3 6 310 157 253 45 141 91 47 228 322 48 130 297 150 273 106 213 159 305"
b = [read x :: Int| x <- (splitOn " " a)]
k_arr x = [foundSort x k | k <- [1..(optima x)]]



