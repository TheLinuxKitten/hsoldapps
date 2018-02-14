pragma solidity ^0.4.15;

contract TypeOps {

  function bytesops0(bytes2 a, bytes2 b)
            public pure returns (bytes1, bytes2, bytes2, bytes2, bytes2) {
    return (bytes1(a), a & b, a | b, a^b, ~a);
  }

  function bytesops1(bytes2 a, bytes2 b, int24 c)
            public pure returns (bytes2, bytes2, bytes2) {
    return (a << 2, a >> 3, bytes2(c));
  }

  function bytesops2(uint24 d, int16 e, uint16 f)
            public pure returns (bytes2, bytes2, bytes2) {
    return (bytes2(d), bytes2(e), bytes2(f));
  }

  function intops0(int16 a, uint16 b) public pure returns (int8, uint8) {
    return (int8(a), uint8(b));
  }

  function intops1a(int8 a, int8 b, uint8 c, uint8 d)
      public pure returns (int8, int8, uint8, uint8) {
    return (a+b, a-b, c+d, c-d);
  }

  function intops1b(int8 a, int8 b, int8 c, int8 d)
      public pure returns (int8, int8) {
    return (a+b+c+d, a-b+c-d);
  }

  function intops1c(uint8 a, uint8 b, uint8 c, uint8 d)
      public pure returns (uint8, uint8) {
    return (a+b+c+d, a-b+c-d);
  }

  function intops2(int16 a, uint16 b)
      public pure returns (int16, int16, int16, int16) {
    return ( a + int16(b)
           , a - int16(b)
           , a ^ int16(b)
           , ~a
           );
  }

  function intops3(int16 a, uint16 b)
      public pure returns (int16, int16, int16, int16) {
    return ( a << 3
           , a >> 5
           , a & int16(b)
           , a | int16(b));
  }

  function intops4(int16 a, uint16 b)
      public pure returns (uint16, uint16, uint16, uint16) {
    return ( uint16(a) + b
           , uint16(a) - b
           , uint16(a) ^ b
           , ~uint16(a)
           );
  }

  function intops5(int16 a, uint16 b)
      public pure returns (uint16, uint16, uint16, uint16) {
    return ( uint16(a) << 3
           , uint16(a) >> 5
           , uint16(a) & b
           , uint16(a) | b
           );
  }

}

