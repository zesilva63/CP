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
    
g1 :: Graph Int
g1 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 1]
           }

g2 :: Graph Int 
g2 = Graph {nodes = fromList [1,2,3,4], 
             edges = fromList [Edge 1 2,Edge 1 3,Edge 2 4,Edge 4 1]
            }          

g3F :: Graph Int 
g3F = Graph {nodes = fromList [1,3,4,5], 
            edges = fromList [Edge 1 2, Edge 2 3, Edge 3 4, Edge 4 1]
            }

g4 :: Graph Int 
g4 = Graph {nodes = fromList [1,2,3,4,5], 
            edges = fromList [Edge 1 2, Edge 2 3, Edge 3 4, Edge 4 5]
            }

g4F :: Graph Int 
g4F = Graph {nodes = fromList [1,2,3,5], 
             edges = fromList [Edge 1 2, Edge 2 3, Edge 3 4, Edge 4 5]
            }

g4x2 :: Graph Int
g4x2 = Graph {nodes = fromList [1,2,3,4,5], 
              edges = fromList [Edge 1 2,Edge 1 3, Edge 2 3, Edge 2 4,Edge 3 4 ,Edge 4 1 ,Edge 4 5]
             } 

g5 :: Graph Int 
g5 = Graph {nodes = fromList [1,2,3,4,5], 
            edges = fromList [Edge 1 2, Edge 2 3, Edge 3 4, Edge 4 5, Edge 5 1]
            }                        

g5T :: Graph Int 
g5T = Graph {nodes = fromList [1,2,3,4,5], 
            edges = fromList [Edge 2 1, Edge 3 2, Edge 4 3, Edge 5 4, Edge 1 5]
            }                        

g6 :: Graph Int 
g6 = Graph {nodes = fromList [1,2,3,4,5], 
            edges = fromList [Edge 1 2, Edge 1 3, Edge 1 4]
            }                 

emptyG :: Graph Int
emptyG = Graph {nodes = fromList[], edges = fromList[]}


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
test_isValid2 = isValid g3F ~?= False

-- Funçao isDAG 
test_isDAG1 :: Test
test_isDAG1 = isDAG g4 ~?= True

test_isDAG2 :: Test
test_isDAG2 = isDAG g2 ~?= False

test_isDAG3 :: Test 
test_isDAG3 = isDAG g4F ~?= False 

-- Funcao isForest
test_isForest1 :: Test
test_isForest1 = isForest g4 ~?= True

test_isForest2 :: Test
test_isForest2 = isForest g2 ~?= False

-- Funcao isSubgraphOf
test_isSubgraphOf1 :: Test
test_isSubgraphOf1 = isSubgraphOf g4 g5 ~?= True 

test_isSubgraphOf2 :: Test
test_isSubgraphOf2 = isSubgraphOf g2 g5 ~?= False

-- Funcao transpose

test_transpose :: Test
test_transpose = transpose g5 ~?= g5T

-- Funcao union 
test_union :: Test
test_union = Graph.union g2 g4 ~?= g4x2

-- Funcao bft
test_bft1 :: Test
test_bft1 = bft g6 (fromList[4]) ~?= Graph {nodes = fromList[4],edges = fromList[]}

test_bft2 :: Test
test_bft2 = bft g6 (fromList[1]) ~?= Graph {nodes = fromList[1,2,3,4],edges = fromList [Edge 2 1,Edge 3 1,Edge 4 1]} 

--test_bft3 :: Test
--test_bft3 = bft 

--test_bft4 :: Test
--test_bft4 = bft 

-- Funcao reachable
test_reachable :: Test
test_reachable = reachable g6 1 ~?= fromList[1,2,3,4]

-- Funcao isPathOf
test_isPathOf1 :: Test
test_isPathOf1 = isPathOf [Edge 1 5,Edge 5 2,Edge 2 4] g6 ~?= False

test_isPathOf2 :: Test
test_isPathOf2 = isPathOf [Edge 1 3] g5 ~?= False

test_isPathOf3 :: Test
test_isPathOf3 = isPathOf [Edge 1 2, Edge 2 3] g4 ~?= True 

-- Funcao path 



main = runTestTT $ TestList [test_adj,test_swap,test_empty,test_isEmpty1,
                             test_isEmpty2,test_isValid1,test_isValid2,
                             test_isDAG1,test_isDAG2,test_isDAG3,test_isForest1,
                             test_isForest2,test_isSubgraphOf1,test_isSubgraphOf2,
                             test_transpose,test_union,test_bft1,test_bft2,test_reachable,
                             test_isPathOf1,test_isPathOf2,test_isPathOf3]

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
    arbitrary = aux `suchThat` isValid
        where aux = do ns <- arbitrary
                       es <- arbitrary
                       return $ Graph {nodes = fromList ns, edges = fromList es}
 
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
