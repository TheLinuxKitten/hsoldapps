## Ejemplos de uso de contracts (Solidity)

Ejemplos de uso de las librerías **hethrpc** y **hethdapp**: creación y
manipulación de contracts **Solidity** en el monad _Web3T_.

Los ejemplos se compilan con la opción `-ddump-splices` que genera ficheros
con la extensión `.dump-splices` que contienen el código generado por _Template
Haskell_.

**Compilar y ejecutar en redes privadas**

### Dependencias

* Compilador _solidity_

* Nodo _Ethereum_ accesible

* Nodo _Swarm_ accesible

Para la ejecución de los programas es necesario que los nodos estén en funcionamiento. Se espera que el nodo tenga un mínimo de cuatro _unlocked accounts_ con sufiente cantidad de _ethers_.

### Ejemplo de uso

Crear los _contracts_ del ejemplo con el comando
```
$ hsoldapp1 {--http <url>|--ipc <geth.ipc>} --log --doOk +RTS -N4
```

El comando produce al final de su salida un listado con las direcciones de los _contracts_ en el blockchain. Esa información se puede usar para sucesivas llamadas:
```
$ hsoldapp1 {--http <url>|--ipc <geth.ipc>} --contractAddress "Topics:0x..." --contractAddress "Test1:0x..." --contractAddress "Types:0x..." --contractAddress "New1:0x..." --contractAddress "Coin:0x..." --contractAddress "Sharer:0x..." --contractAddress "OwnedToken:0x..." --contractAddress "Meta:0x..." --contractAddress "Overload:0x..." --log --doOk +RTS -N4
```

