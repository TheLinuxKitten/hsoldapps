
pragma solidity ^0.4.15;

import "github.com/modular-network/ethereum-libraries/StringUtilsLib/StringUtilsLib.sol";
import "github.com/TheLinuxKitten/ethlibs/owned.sol";
import "github.com/TheLinuxKitten/ethlibs/priced.sol";

contract Test1 is Owned, Priced {

  string public nombre;
  mapping (address => uint) public tokens;
  uint public tokenPrice = 5;

  event Nombre(string s1, string indexed s2);
  event TokenPrice(uint indexed p);
  event Buy(address indexed a, uint b);
  event SendTokens(address indexed f, address indexed t, uint a);

  function setNombre(string prefijo, string _nombre) private onlyOwner {
    nombre = StringUtilsLib.concat(StringUtilsLib.toSlice(prefijo),StringUtilsLib.toSlice(_nombre));
    Nombre(nombre, nombre);
  }

  function setTokenPrice(uint price) private onlyOwner {
    tokenPrice = price;
    TokenPrice(tokenPrice);
  }

  function Test1(string s) public {
    owner = msg.sender;
    setNombre("Contract: ", s);
    setTokenPrice(5);
  }

  function cambiaNombre(string s) public onlyOwner {
    setNombre("Contract actualizado: ", s);
  }

  function buyTokens() public payable costs(tokenPrice) {
    uint amount = msg.value/tokenPrice;
    tokens[msg.sender] += amount;
    Buy(msg.sender,amount);
  }

  function sendTokens(address to, uint amount) public {
    if (tokens[msg.sender] >= amount) {
      tokens[to] += amount;
      tokens[msg.sender] -= amount;
      SendTokens(msg.sender, to, amount);
    }
  }

  function cambiaTokenPrice(uint price) public onlyOwner {
    setTokenPrice(price);
  }

}

