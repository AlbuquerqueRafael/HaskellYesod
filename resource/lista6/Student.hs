module Student(insert, remove, search, union, intersection, minus, inclusion, summy, size) where

import qualified Data.List as List

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert e ([]) = [(e, 1)]
insert e ((x:xs))
  |(fst x == e) = [(fst x, (snd x)+1)] ++ xs
  |otherwise = [x] ++ (insert e (xs))


{-- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura.
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura-}

remove e ([]) = []
remove e ((x:xs))
  |(fst x == e) =
    if((snd x) == 1) then
      xs
    else
      [(fst x, (snd x)-1)] ++ xs
  |otherwise = [x] ++ (remove e (xs))

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search :: Eq a => a -> [(a,Int)] -> Int
search e [] = 0
search e (x:xs)
  |((fst x) == e) = (snd x)
  |otherwise = (search e (xs))

{-
 - Faz a uniao deste com other A uniao consiste em ter os elementos dos dois  com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
union ([]) (ys) = ys
union ((x:xs)) (ys) =
  let elementIn = search (fst x) (ys) in
  if (snd x) > elementIn then
    [x] ++ union (xs) ((filter (\tup -> (fst tup) /= (fst x)) ys))
  else
    union (xs) (ys)

{-
 - Faz a intersecao deste com other A intersecao consiste em ter os elementos que estao em ambos os bags com suas
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B entao a intersecao deixa A vazio.
-}
intersection ([]) (ys) = []
intersection (xs) ([]) = []
intersection ((x:xs)) (ys) =
  let elementIn = search (fst x) (ys) in
  if elementIn > 0 then
    if elementIn > (snd x) then
      [x] ++ intersection (xs) ((filter (\tup -> (fst tup) /= (fst x)) ys))
    else
      [(fst x, elementIn)] ++ intersection (xs) ((filter (\tup -> (fst tup) /= (fst x)) ys))
  else
    intersection (xs) (ys)

{-
 - Faz a diferenca deste com other A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B).
     Caso essa quantidade seja negativa o elemento deve serremovido do
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus ([]) (ys) = []
minus (xs) ([]) = xs
minus (x:xs) (ys) =
  let elementIn = search (fst x) (ys)
      differenceBetweenElements = (snd x) - elementIn in
  if differenceBetweenElements > 0 then
    [(fst x, differenceBetweenElements)] ++ minus (xs) ((filter (\tup -> (fst tup) /= (fst x)) ys))
  else
    minus (xs) (ys)

{-
 - Testa se este esta incluso em other Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em other
-}
inclusion (xs) ([]) = False
inclusion ([]) (ys) = True
inclusion ((x:xs)) (ys) =
  let elementIn = search (fst x) (ys) in
  if elementIn >= (snd x) then
    inclusion ( xs) (ys)
  else
    False

{-
 - Realiza a soma deste com other A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas.
-}
{-TODO: lembrar de falar com Adalberto sobre a necessidade do nome ser summy.-}
summy [] ys = ys
summy (x:xs) ys =
  let elementIn = search (fst x) (ys)
      sumElements = (snd x) + elementIn in
  [(fst x, sumElements)] ++ summy (xs) ((filter (\tup -> (fst tup) /= (fst x)) ys))

{-
 - Retorna a quantidade total de elementos no -}
size ([]) = 0
size (xs) = foldl (\a (b, c) ->  a + c) 0 xs
