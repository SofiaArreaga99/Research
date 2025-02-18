Meaning of the variables 

test = Matrix

dbhBlkSpruce = Random Dbhs over the range sampled

meany =	biomass using the pulished equation

ys	= Create fuzzies the biomasses to create 1000 different populations

stdevs	= sequence this progresions will create 1000 diffent iomass sets

stdevs2 	=la matriz elaborada con l resultado de stdevs 

dbh2	= es una matriz donde estan los randoms DBHs 

psuedys	= Aquí se produce la nueva biomasa si no hay heteroscedasticity 

rsq2	= memory allocation, speeds up 

sst	= suma de cuadrados total se usa la nueva biomasa calculada

sse	= suma de cuadrados del error

rsq2[i]	= coeficiente de determinación de R2

rsqp	= Published R2 value 

diffs	= diferencia entre el R calculado y el obtenido

I	= Find the index of the minimum value

BMBlkSpruce	= psuedys[, I]  # Select corresponding column
