pragma solidity ^0.4.0;

contract Sharer {

    event SendHalf(address indexed from, address indexed to, uint value);

    function sendHalf(address addr) public payable returns (uint balance) {
      require(msg.value % 2 == 0);
      uint balanceBeforeTransfer = this.balance;
      addr.transfer(msg.value / 2);
      assert(this.balance == balanceBeforeTransfer - msg.value / 2);
      SendHalf(msg.sender, addr, msg.value);
      return this.balance;
    }

}

