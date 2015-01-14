; Yuan Fang Fu
; cs61a-ae
; TA: Darren Kuo
; Section: 13 Group: 08

; Ex. 3.38

; a) 3! unique combinations, although Peter and Paul are exchangeable when done following one another. 3!-2! = 4 values

; b) If all three concurrently eval'd then 3P1, 3!/(3-1)! or three different values possible

; two concurrent and 1 sequential --> essentially 2 are eval'd --> 3P2, or 6 ways, with a pair of redundant cases when Peter and Paul are eval'd, for a total of 5

; the remaining ones are covered in part a --> 4 more

; I guessed 5+4+3, for 12 different numbers; checked to find 10

; Ex. 3.39

; 2 remain, 101, and 121

; Ex. 3.40

; when sequential --> (10^2)^3 = 10^6 , order doesn't matter

; interleaved:

; 10 * (10)^3 = 10^4; 10 * (10^2) * (10^2) = 10^5; 10 * 10 * (10^2) = 10^4;

; 10^2, 10^3, 10^4, 10^5, 10^6 possible

; Ex. 3.41

; There is no need for Ben's change, since serializing the mutation methods are enough to make any operations sequential. Calls to balance don't have the delay between the set! arguments deposit or withdraw has, and doesn't need to be serialized.

; Ex. 3.42

; There might be a problem with simultaneous withdraw and deposits. Since the let created two separate serialed procedures in withdraw and desposit, each with its own serializer. This results in all deposits, or withdraws sequential in raltion to each other, but interleaving can still occur between the two method calls

; Ex. 3.44

; It should not be a problem; in the exhange case, there is the let definiton of difference whose evaluation causes interleaving problems. There's no such intermediate step in transfer

; Ex. 3.46

; Not really sure what the timing diagrams do to help....

; basically, while the mutex is evaluated before the first procedure can begin, there is a delay between the testing for #f, and the setting of the cell to true. During this delay another procedure call may still pass the predicate for an empty mutex, and concurrent evaluation occurs.

; Ex. 3. 48

