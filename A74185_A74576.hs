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

-- Grafo não válido, pois o nodo 4 nao se encontra na lista mas faz parte dos vertices.
--g4F :: Graph Int 
--g4F = Graph {nodes = fromList [1,2,3,5],
--             edges = fromList [Edge 1 2, Edge 2 3, Edge 3 4, Edge 4 5]
--            }

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
    arbitrary = do es <- arbitrary
                   return $ Graph {nodes = juntaNodos(es), edges = fromList es}
                   where
                      juntaNodos x = Set.map source (fromList x) `Set.union` Set.map target (fromList x)

prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

-- Gerador de DAGs
dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = arbitrary `suchThat` isDAG

prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g

-- Gerador de florestas
forest :: (Ord v, Arbitrary v) => Gen (Forest v)
forest = arbitrary `suchThat` isForest

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
prop_swap e = property $ swap(swap e) == e

-- Funcao emtpy
prop_empty :: Property
prop_empty = property(Set.null (nodes (Graph.empty)))  

-- Funcao isEmpty
prop_isEmpty :: Graph Int -> Bool
prop_isEmpty g = if (isEmpty g)
                 then g == Graph.empty
                 else length(elems(nodes g)) /= 0

-- Funcao isValid
check :: [Int] -> Edge Int -> Bool
check [] x = False
check (h:t) x = if(h == source x )
                then True 
                else if (h == target x)
                     then True
                     else check t x

prop_isValid :: Graph Int -> Property   
prop_isValid g = property((isValid g) == (all ( \v -> check (elems(nodes g)) v) (elems(edges g))))

-- Funcao isDAG

verifica :: [Edge Int] -> [Int] -> Bool
verifica [] l = True 
verifica (h:t) l = if(elem (source h) l)
                   then False
                   else verifica t ((source h):l)

prop_isDAG :: Graph Int -> Bool 
prop_isDAG g = isDAG g == (isValid g && (verifica (elems(edges g)) []))
              
-- Funcao isForest

prop_isForest :: Graph Int -> Bool
prop_isForest g = isForest g == (isDAG g && (all (\v -> length(Prelude.map target (elems(adj g v)))<2) (nodes g)) )

-- Funcao isSubgraphOf 

prop_isSubgraphOf :: Graph Int -> Graph Int -> Bool
prop_isSubgraphOf g1 g2 = (isSubgraphOf g1 g2 ) == ( all(\v -> v `elem`(elems(nodes g2))) (elems(nodes g1)) && all(\v -> v `elem`(elems(edges g2))) (elems(edges g1)) )

-- Funcao transpose
prop_transpose :: Graph Int -> Property
prop_transpose g = property $ transpose(transpose g) == g

-- Funcao union
prop_union :: Graph Int -> Graph Int -> Bool
prop_union g1 g2 = (all(\e -> elem e (edges(Graph.union g1 g2))) (edges g1) ) && (all(\e -> elem e (edges(Graph.union g1 g2))) (edges g2)) -- verificar que pode ter elementos a mais

-- Funcao bft
--prop_bft :: Graph Int -> Set Int -> Bool -- funcao a pecorre caminho , caso nao exista tem dar uma lista vazia !
--prop_bft g v = f (elems(nodes(g))) (elems(edges(bft g v))) v
--        where f n [] x = 

-- Funcao reachable
prop_reachable :: Graph Int -> Int -> Bool
prop_reachable g v = if(isEmpty(g) || elems(edges(g))==[])
                     then True  
                     else all (\x -> elems(nodes(bft g (singleton x)))/=[] ) (elems(reachable g v)) 

-- Funcao isPathOf
prop_isPathOf :: Graph.Path Int -> Graph Int -> Bool
prop_isPathOf l g = all (\v-> (v `elem` elems(edges g)) == (isPathOf l g)) l 

-- Funcao path
--path :: Graph Int -> Int -> Int -> Bool
--path g o d =  

-- Funcao topo 




