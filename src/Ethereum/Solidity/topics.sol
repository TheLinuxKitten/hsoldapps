pragma solidity ^0.4.16;

contract Topics {

  event EvBytes(bytes b1, bytes indexed b2);
  event EvString(string b1, string indexed b2);
  event EvArrUint(uint[] b1, uint[] indexed b2);
  event EvArrUint48(uint48[] b1, uint48[] indexed b2);
  event EvArrBool(bool[] b1, bool[] indexed b2);
  event EvArrBytes16(bytes16[] b1, bytes16[] indexed b2);
  event EvArrBytes32(bytes32[] b1, bytes32[] indexed b2);

  function sendEvBytes(bytes b1) public { EvBytes(b1, b1); }
  function sendEvString(string b1) public { EvString(b1, b1); }
  function sendEvArrUint(uint[] b1) public { EvArrUint(b1, b1); }
  function sendEvArrUint48(uint48[] b1) public { EvArrUint48(b1, b1); }
  function sendEvArrBool(bool[] b1) public { EvArrBool(b1, b1); }
  function sendEvArrBytes16(bytes16[] b1) public { EvArrBytes16(b1, b1); }
  function sendEvArrBytes32(bytes32[] b1) public { EvArrBytes32(b1, b1); }
}

