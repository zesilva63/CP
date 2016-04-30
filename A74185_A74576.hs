--
-- Projecto CP 2015/16
--
-- O projecto consiste em desenvolver testes para o módulo Graph.hs
-- (para grafos orientados e não pesados).
-- Mais concretamente, o projecto consiste em 3 tarefas que são descritas abaixo.
-- O prazo para entrega é o dia 3 de Abril. Cada grupo deve enviar apenas
-- o módulo de testes (este módulo) por email para calculodeprogramas@gmail.com
-- O nome do ficheiro deve identificar os números dos 2 alunos do grupo (numero1_numero2.hs).
-- Certifiquem-se que o módulo de testes desenvolvido compila correctamente antes
-- de submeter. O módulo Graph.hs não deve ser alterado.
-- Os 2 alunos do grupo devem também indentificar-se nos comentários abaixo.
--
-- Aluno 1
-- Número: A74576
-- Nome: José António Dantas Silva
-- Curso: Mestrado Integrado em Engenharia Informática 
--
-- Aluno 2
-- Número: A74185
-- Nome: Ricardo António Gonçalves Pereira
-- Curso: Mestrado Integrado em Engenharia Informática
--


module Main where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set
import Data.Maybe

--
-- Teste unitário
--
    
-- emptyG -> Grafo vazio.
emptyG :: Graph Int
emptyG = Graph {nodes = fromList[], edges = fromList[]}


-- G1 -> Grafo de apenas um vertice cuja aresta é ciclica.
g1 :: Graph Int
g1 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 1]
           }

-- Grafo válido e ciclico.
g2 :: Graph Int 
g2 = Graph {nodes = fromList [1,2,3,4], 
            edges = fromList [Edge 1 2,Edge 1 3,Edge 2 4,Edge 4 1]
           }         

-- Grafo válido e ciclico, transposto do grafico g2.
g2T :: Graph Int 
g2T = Graph {nodes = fromList [1,2,3,4], 
             edges = fromList [Edge 2 1,Edge 3 1,Edge 4 2,Edge 1 4]
            }         

-- Grafo não válido, pois o nodo 2 nao se encontra na lista mas faz parte dos vertices.
g2F :: Graph Int 
g2F = Graph {nodes = fromList [1,3,4,5], 
             edges = fromList [Edge 1 2, Edge 1 3, Edge 2 4, Edge 4 1]
            }

-- Subgrafo válido do grado g2
g2S :: Graph Int
g2S = Graph {nodes = fromList[1,2,4],
             edges = fromList[Edge 1 2, Edge 2 4, Edge 4 1]
            }

g3 :: Graph Int
g3 = Graph {nodes = fromList [1,2,3,4,5],
            edges = fromList [Edge 1 2, Edge 1 3, Edge 2 4, Edge 3 5]
           }

-- Grafo válido não ciclico que forma uma linha entre os vertices 1 e 5 ordenadamente.
g4 :: Graph Int 
g4 = Graph {nodes = fromList [1,2,3,4,5], 
            edges = fromList [Edge 1 2, Edge 2 3, Edge 3 4, Edge 4 5]
            }
-- Grafo que contem o resultado da junção do grafo g4 com o grafo g2
g4x2 :: Graph Int
g4x2 = Graph {nodes = fromList [1,2,3,4,5], 
              edges = fromList [Edge 1 2,Edge 1 3, Edge 2 3, Edge 2 4,Edge 3 4 ,Edge 4 1 ,Edge 4 5]
             } 

-- Grrafo ciclico circular entre os vertices 1 e 5.
g5 :: Graph Int 
g5 = Graph {nodes = fromList [1,2,3,4,5], 
            edges = fromList [Edge 1 2, Edge 2 3, Edge 3 4, Edge 4 5, Edge 5 1]
            }                        
                       

g6 :: Graph Int 
g6 = Graph {nodes = fromList [1,2,3,4], 
            edges = fromList [Edge 1 2, Edge 1 3, Edge 1 4]
            }                 

-- Aresta para teste entre vertice 1 e 2.
e1 :: Edge Int 
e1 = Edge 1 2

-- Um exemplo de um teste unitário.
test_adj :: Test
test_adj = adj g1 1 ~?= fromList [Edge 1 1]


--
-- Tarefa 1
--
-- Defina testes unitários para todas as funções do módulo Graph,
-- tentando obter o máximo de cobertura de expressões, condições, etc.
--

-- Funçao swap
test_swap :: Test 
test_swap = swap e1 ~?= Edge 2 1  

-- Função empty 
test_empty :: Test
test_empty = Graph.empty ~?= emptyG

-- Função isEmpty
test_isEmpty1 :: Test
test_isEmpty1 = isEmpty g1 ~?= False

test_isEmpty2 :: Test
test_isEmpty2 = isEmpty emptyG ~?= True 

-- Função isValid
test_isValid1 :: Test
test_isValid1 = isValid g2 ~?= True

test_isValid2 :: Test
test_isValid2 = isValid g2F ~?= False

-- Funçao isDAG 
test_isDAG1 :: Test
test_isDAG1 = isDAG g2 ~?= False

test_isDAG2 :: Test
test_isDAG2 = isDAG g2F ~?= False

test_isDAG3 :: Test 
test_isDAG3 = isDAG g3 ~?= True

-- Funcao isForest
test_isForest1 :: Test
test_isForest1 = isForest g4 ~?= True

test_isForest2 :: Test
test_isForest2 = isForest g2 ~?= False

test_isForest3 :: Test
test_isForest3 = isForest g3 ~?= False

-- Funcao isSubgraphOf
test_isSubgraphOf1 :: Test
test_isSubgraphOf1 = isSubgraphOf g2S g2 ~?= True 

test_isSubgraphOf2 :: Test
test_isSubgraphOf2 = isSubgraphOf g2S g3 ~?= False

-- Funcao transpose
test_transpose :: Test
test_transpose = transpose g2 ~?= g2T

-- Funcao union 
test_union :: Test
test_union = Graph.union g2 g4 ~?= g4x2

-- Funcao bft
test_bft1 :: Test
test_bft1 = bft emptyG (fromList[]) ~?= Graph {nodes = fromList[],edges = fromList[]}

test_bft2 :: Test
test_bft2 = bft g6 (fromList[1]) ~?= Graph {nodes = fromList[1,2,3,4],edges = fromList [Edge 2 1,Edge 3 1,Edge 4 1]} 

-- Funcao reachable
test_reachable :: Test
test_reachable = reachable g2 2 ~?= fromList[1,2,3,4]

test_reachable2 :: Test
test_reachable2 = reachable g2 3 ~?= fromList[3]

-- Funcao isPathOf
test_isPathOf1 :: Test
test_isPathOf1 = isPathOf [Edge 1 5,Edge 5 2,Edge 2 4] g6 ~?= False

test_isPathOf2 :: Test
test_isPathOf2 = isPathOf [Edge 1 3] g5 ~?= False

test_isPathOf3 :: Test
test_isPathOf3 = isPathOf [Edge 1 2, Edge 2 3] g4 ~?= True 

test_isPathOf4 :: Test
test_isPathOf4 = isPathOf [] g4 ~?= True 

-- Funcao path 
test_path1 :: Test 
test_path1 = path g4x2 1 4 ~?= Just [Edge 1 2,Edge 2 4]

test_path2 :: Test 
test_path2 = path g4x2 5 1 ~?= Nothing 


-- Funcao topo VER MELHOR!!!!
test_topo1 :: Test
test_topo1 = topo g4 ~?= [fromList [1],fromList [2],fromList [3],fromList [4],fromList [5]]

test_topo2 :: Test
test_topo2 = topo g6 ~?= [fromList [1],fromList [2,3,4]]




main = runTestTT $ TestList [test_adj,test_swap,test_empty,test_isEmpty1,
                             test_isEmpty2,test_isValid1,test_isValid2,
                             test_isDAG1,test_isDAG2,test_isDAG3,test_isForest1,
                             test_isForest2,test_isForest3 ,test_isSubgraphOf1,
                             test_isSubgraphOf2, test_transpose,test_union,
                             test_bft1,test_bft2,test_reachable, test_reachable2, 
                             test_isPathOf1,test_isPathOf2,test_isPathOf3, test_isPathOf4,
                             test_path1, test_path2, test_topo1, test_topo2]


--
-- Teste aleatório
--

--
-- Tarefa 2
--
-- A instância de Arbitrary para grafos definida abaixo gera grafos
-- com muito poucas arestas, como se pode constatar testando a
-- propriedade prop_valid.
-- Defina uma instância de Arbitrary menos enviesada.
-- Este problema ainda é mais grave nos geradores dag e forest que
-- têm como objectivo gerar, respectivamente, grafos que satisfazem
-- os predicados isDag e isForest. Estes geradores serão necessários
-- para testar propriedades sobre estas classes de grafos.
-- Melhore a implementação destes geradores por forma a serem menos enviesados.
--


-- Instância de Arbitrary para arestas
instance Arbitrary v => Arbitrary (Edge v) where
    arbitrary = do s <- arbitrary
                   t <- arbitrary
                   return $ Edge {source = s, target = t}

instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
    arbitrary = do n <- arbitrary
                   let nodos = fromList n
                   if(Set.null nodos) then return $ Graph {nodes = Set.empty, edges = Set.empty }
                   else do arestas <- listOf(criaEdges n) 
                           return $ Graph {nodes = nodos, edges = fromList(arestas) }
                           where
                              criaEdges x = do e1 <- elements(x)
                                               e2 <- elements(x)
                                               return (Edge e1 e2)


prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

-- Gerador de DAGs

dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = do n <- choose(0,20)
         nodos <- arbitrary
         if(Prelude.null nodos) then return $ Graph {nodes = Set.empty, edges = Set.empty }
         else gera_dag (Graph {nodes = fromList(nodos),edges = Set.empty}) n

gera_dag :: (Ord v,Arbitrary v) => Graph v -> Int -> Gen (DAG v)
gera_dag grafo 0 = do return grafo
gera_dag grafo n = do aresta <- criaEdge(toList(nodes grafo))
                      let nGrafo = Graph (nodes grafo) (insert aresta (edges grafo))
                      if(isDAG nGrafo)
                      then gera_dag nGrafo (n-1)
                      else gera_dag grafo (n-1)
                          where
                             criaEdge x = do e1 <- elements(x)
                                             e2 <- elements(x)
                                             return (Edge e1 e2)


prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g



-- Gerador de florestas

forest :: (Ord v, Arbitrary v) => Gen (Forest v)
forest = do n <- choose(0,50)
            nodos <- arbitrary
            if(Prelude.null nodos) then return $ Graph {nodes = Set.empty, edges = Set.empty }
            else gera_forest (Graph {nodes = fromList(nodos),edges = Set.empty}) n

gera_forest :: (Ord v,Arbitrary v) => Graph v -> Int -> Gen (DAG v)
gera_forest grafo 0 = do return grafo
gera_forest grafo n = do aresta <- criaEdge(toList(nodes grafo))
                         let nGrafo = Graph (nodes grafo) (insert aresta (edges grafo))
                         if(isForest nGrafo)
                         then gera_forest nGrafo (n-1)
                         else gera_forest grafo (n-1)
                             where
                                criaEdge x = do e1 <- elements(x)
                                                e2 <- elements(x)
                                                return (Edge e1 e2)

prop_forest :: Property
prop_forest = forAll (forest :: Gen (Forest Int)) $ \g -> collect (length (edges g)) $ isForest g

--
-- Tarefa 3
--
-- Defina propriedades QuickCheck para testar todas as funções
-- do módulo Graph.
-- 

-- Exemplo de uma propriedade QuickCheck para testar a função adj          
prop_adj :: Graph Int -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g

-- Funcao swap
prop_swap :: Edge Int -> Property
prop_swap e = swap(swap e) == e .&&. source e == target(swap e) .&&. target e == source(swap e)

-- Funcao emtpy
prop_empty :: Property
prop_empty = property(Set.null (nodes (Graph.empty)))  

-- Funcao isEmpty
prop_isEmpty :: Graph Int -> Property
prop_isEmpty g = property( isEmpty g == Set.null(nodes g)  )

-- Funcao isValid

prop_isValid :: Graph Int -> Property   
prop_isValid g | Set.null(edges g) = label "null" True  
               | otherwise = isValid g ==>  (forAll (elements $ elems $ edges g) $ \v -> ((source v) `elem` (elems(nodes g)) .&&. (target v) `elem`  (elems(nodes g))) )

-- Funcao isDAG

prop_isDAG :: Graph Int -> Property
prop_isDAG g  | Set.null(edges g) = label "null" True  
              | otherwise = isDAG g ==> (forAll (elements $ elems $ edges g) $ \v -> forAll( elements $ elems (reachable g (target v))) $ \n -> not( (source v) `elem` elems(reachable g  n)))
              
-- Funcao isForest

prop_isForestAux :: DAG Int -> Property
prop_isForestAux g | Set.null(nodes g) = label "null" True  
                   | otherwise = isForest g ==>  ( forAll (elements $ elems $ nodes g) $ \v -> length(elems(adj g v))<2) 

prop_isForest :: Property
prop_isForest = forAll (dag :: Gen (DAG Int)) $ \g -> prop_isForestAux g

-- Funcao isSubgraphOf 
sub :: (Ord v,Arbitrary v) => Graph v -> Gen (Graph v)
sub grafo = do novos <- sublistOf(Set.elems(nodes(grafo)))
               let ed = sub_aux (Set.elems(edges(grafo))) novos
               return Graph {nodes = fromList(novos), edges = fromList ed }
                
sub_aux :: Eq v => [Edge v] -> [v] -> [Edge v]
sub_aux [] lista = []
sub_aux ((Edge x y):t) lista | ((elem x lista) && (elem y lista)) = (Edge x y) : sub_aux t lista
                             | otherwise = sub_aux t lista

prop_isSubgraphOfAux :: Graph Int -> Graph Int -> Property 
prop_isSubgraphOfAux g1 g2 = isSubgraphOf g1 g2  ==> (nodes g1 `isSubsetOf` nodes g2) .&&. (edges g1 `isSubsetOf` edges g2) 

prop_isSubgraphOf :: Graph Int -> Property
prop_isSubgraphOf g = forAll(sub g :: Gen (Graph Int)) $ \g1 -> prop_isSubgraphOfAux g1 g

-- Funcao transpose
prop_transpose :: Graph Int -> Property
prop_transpose g = property $ transpose(transpose g) == g  

prop_transpose2 :: Graph Int -> Property
prop_transpose2 g | Set.null(edges g) = label "null" True
                  | otherwise = forAll (elements $ elems $ edges(transpose g)) $ \v -> singleton(swap v) `isSubsetOf` edges g

-- Funcao union
prop_union :: Graph Int -> Graph Int -> Property
prop_union g1 g2 | Set.null(edges g1) || Set.null(edges g2) = label "null" True  
                 | otherwise = (forAll (elements $ elems $ edges g1) $ \e ->  e `elem` (edges(Graph.union g1 g2)) )  .&&. ( forAll (elements $ elems $ edges g2) $ \e -> e `elem` (edges(Graph.union g1 g2)) ) 

-- Funcao bft 

prop_bft :: Graph Int  -> Property
prop_bft g   | Set.null(nodes g) = label "null" True
             | otherwise = forAll (sub g :: Gen (Graph Int)) $ \g1 -> do let cam = elems $ edges $ bft g (nodes g1)
                                                                         if(Prelude.null cam) 
                                                                         then property (True)  
                                                                         else forAll (elements $ cam) $ \e -> (swap e) `elem` elems(edges g) .&&. (nodes g1) `isSubsetOf` (nodes g)

prop_bft2 :: Graph Int -> Property
prop_bft2 g = forAll(sub g :: Gen (Graph Int)) $ \g1 -> isForest $ bft g (nodes g1)

-- Funcao reachable
prop_reachable :: Graph Int -> Property
prop_reachable g   | Set.null(edges g) = label "null" True  
                   | otherwise = forAll (elements $ elems $ nodes g) $ \v -> forAll (elements $ elems $ reachable g v) $ \e -> elems(nodes(bft g (singleton e)))/=[]  

-- Funcao isPathOf
prop_isPathOf :: Graph Int -> Property
prop_isPathOf  g  | Set.null(nodes g) = label "null" True
                  | otherwise = forAll (elements $ elems $ nodes g) $ \o-> forAll (elements $ elems $ nodes g) $ \d-> if(path g o d /= Nothing) 
                                                                                                                      then isPathOf (fromJust(path g o d)) g
                                                                                                                      else isPathOf [] g

-- Funcao path
prop_path :: Graph Int -> Property
prop_path g  | Set.null(nodes g) = label "null" True
             | otherwise = forAll (elements $ elems $ nodes g) $ \o-> forAll (elements $ elems $ nodes g) $ \d-> do let cam = path g o d
                                                                                                                    if(cam /= Nothing && fromJust cam /= []) 
                                                                                                                    then forAll (elements $ fromJust cam) $ \ e ->(singleton e) `isSubsetOf` (edges g)
                                                                                                                    else property(True)
prop_path2 :: Graph Int -> Int -> Int -> Property
prop_path2 g o d = if(path g o d /= Nothing)
                   then isPathOf (fromJust(path g o d)) g .&&. d `elem` elems(reachable g o)
                   else property (d `notElem` elems(reachable g o) )

-- Funcao topo
check :: [Set Int] -> DAG Int -> Property
check [h] g = property (True)
check (h1:t) g = forAll (elements $ elems $ h1) $ \v1 -> if(not(Set.null (adj g v1)))
                                                          then forAll (elements $ Prelude.map (target) (elems $ adj g v1)) $ \v2 -> v2 `elem` converte (Prelude.map (elems) t) .&&. check t g
                                                          else property (True)
              where converte [] =[]
                    converte (h:t) = h++converte t 

prop_topoAux :: DAG Int -> Property
prop_topoAux g | Set.null (nodes g) || Set.null(edges g) = label "null" True
               | otherwise = check (topo g) g

prop_topo :: Property
prop_topo = forAll (dag :: Gen (DAG Int)) $ \g -> prop_topoAux g

