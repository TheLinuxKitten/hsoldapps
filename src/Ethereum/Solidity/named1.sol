pragma solidity ^0.4.0;

contract C {
  event F(uint indexed key, uint indexed value);

  function f(uint key, uint value) public {
    F(key,value);
  }

  function g() public {
    f({value: 2, key: 3});
  }
}

