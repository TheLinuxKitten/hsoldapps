
pragma solidity ^0.4.18;

contract Overload {

  string public cadena1;

  function funcion(bool a) public pure returns (bool) {
    return a;
  }

  function funcion(string a) public {
    cadena1 = a;
  }

  function funcion(uint b, string a) public returns (uint) {
    cadena1 = a;
    return b;
  }

  function func2(bool a) public pure returns (bool) {
    return a;
  }
}

