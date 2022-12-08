# Shiny_app_MNIST

## Prediccion de numeros escritos a mano 

Este proyecto emplea una shiny app para realizar predicciones de numeros a mano usando como modelos predictivos Support Vector Machines y Random Forest. ambos modelos fueron entrenados usando el data set de MNIST y para entrenarlos se hizo una transformacion PCA sobre las imagenes orignales. 

En la aplicacion el usuario puede introducir un numero escrito a mano haciendo un click y cerrando la escritura con otro click en la pantalla que indica: ingresar un numero a mano. Posteriormente, la aplicacion devuelve una prediccion del numero insertado y guarda la inormacion haciendo uso de una api en python. 

