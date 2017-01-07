module Test where

import qualified Data.Vector    as V

import           NeuralNetworks

test1 :: IO ()
test1 = do
    a <- nn [5, 2]
    print a
    let input1 = V.fromList [1, 1, 0, 0, 1]
        expected1 = V.fromList [0, 1]
        input2 = V.fromList [1, 0, 1, 1, 0]
        expected2 = V.fromList [1, 0]
        input3 = V.fromList [0, 1, 0, 1, 0]
        expected3 = V.fromList [0, 1]
        input4 = V.fromList [1, 1, 1, 1, 0]
        expected4 = V.fromList [1, 0]
        train nn1 = do
            let grads1 = backpropGradient nn1 input1 expected1
                grads2 = backpropGradient nn1 input2 expected2
                grads3 = backpropGradient nn1 input3 expected3
                grads4 = backpropGradient nn1 input4 expected4
                mGrad = totalGradient [grads1, grads2, grads3, grads4]
            return $ backpropCorrection 0.01 mGrad nn1
        go 0 nn1 = return nn1
        go n nn1 = do
            nn2 <- train nn1
            go (n - 1) nn2
    nn2 <- go 100000 a
    let outputs1 = V.last $ forwardScanl nn2 input1
        outputs2 = V.last $ forwardScanl nn2 input2
        outputs3 = V.last $ forwardScanl nn2 input3
        outputs4 = V.last $ forwardScanl nn2 input4
    putStrLn $ "output1: " ++ show outputs1
    putStrLn $ "output2: " ++ show outputs2
    putStrLn $ "output3: " ++ show outputs3
    putStrLn $ "output4: " ++ show outputs4
    putStrLn ""
