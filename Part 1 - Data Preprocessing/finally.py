# Data Preprocessing

# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Importing the dataset
dataset = pd.read_csv('Data.csv')
X = dataset.iloc[:, 0:-1].values
y = dataset.iloc[:, 3].values

# Taking care of missing data = sem dado, o subsituindo pela media, mediana ou dado mais recorrente
from sklearn.preprocessing import Imputer
imputer = Imputer(missing_values = 'NaN', strategy = 'mean', axis = 0)
# fit e usado para extrair alguma informacao dos dados
imputer = imputer.fit(X[:, 1:3])
# fazer uma transformacao
X[:, 1:3] = imputer.transform(X[:, 1:3])

# Encoding categorical data = transformar dados nao numerico em numeros
# Encoding the Independent Variable
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
labelencoder_X = LabelEncoder()
# fit transform : strings -> inteiros
X[:, 0] = labelencoder_X.fit_transform(X[:, 0])
# OneHotEncoder sera aplicado apenas em variaveis categoricas que nao expressam valor numerico
# OneHotEncoder cria colunas separadas para cada label com valores 0 e 1
# Ex: yes ou nao pode ser expressa como 0 e 1, assim como tamanho de uma camiste P, G e GG
onehotencoder = OneHotEncoder(categorical_features = [0])
X = onehotencoder.fit_transform(X).toarray()
# Encoding the Dependent Variable
labelencoder_y = LabelEncoder()
y = labelencoder_y.fit_transform(y)

# Splitting the dataset into the Training set and Test set
from sklearn.cross_validation import train_test_split 
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 0)

# Feature Scaling = diminuir a disparidade de valores das variaves
# aumentar a acuracia
# isso nao e aplicado em y por que e variavel dependente com valores de  0 e 1, ou seja nao tem um range grande
"""
from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
X_train = sc_X.fit_transform(X_train)
X_test = sc_X.transform(X_test)
"""