from hypothesis import given, reproduce_failure, settings
from hypothesis.strategies import integers
import numpy as np
import unittest

from u256_test_harness import u256_test_harness

max_value = 2**256 - 1
min_value = 0
mask = 2**32 - 1

o = u256_test_harness()

def encode(n):
    assert n <= max_value and n >= min_value
    ret = []
    for i in range(8):
        ret.append((n >> (i * 32)) & mask)
    return ret

def decode(xs):
    assert len(xs) == 8
    ret = 0
    for i in range(8):
        assert xs[i] <= mask and xs[i] >= 0
        ret |= xs[i] << (i * 32)
    return ret

def to_fut(xs):
    return np.array(xs, dtype=np.uint32)

def of_fut(xs):
    if 'get' in dir(xs):
        return xs.get().tolist()
    else:
        return xs.tolist()

def call2(f, a, b):
    arg_a = encode(a)
    arg_b = encode(b)
    res = of_fut(f(to_fut(arg_a), to_fut(arg_b)))
    #print("input", arg, decode(arg))
    #print("output", res, decode(res))
    return decode(res)

def call1(f, a):
    arg = encode(a)
    res = of_fut(f(to_fut(arg)))
    #print("input", arg, decode(arg))
    #print("output", res, decode(res))
    return decode(res)

def call2(f, a, b):
    return decode(of_fut(f(to_fut(encode(a)), to_fut(encode(b)))))

class Tests(unittest.TestCase):
    @given(n=integers(min_value = min_value, max_value=max_value))
    def test_encode_decode_roundtrip(self, n):
        self.assertEqual(decode(encode(n)), n)

    @given(a=integers(min_value=min_value, max_value=max_value), b=integers(min_value=min_value, max_value=max_value))
    def test_add(self, a, b):
        self.assertEqual(call2(o.add, a, b), (a + b) % 2**256)

    @given(a=integers(min_value=min_value, max_value=max_value), b=integers(min_value=min_value, max_value=max_value))
    def test_sub(self, a, b):
        self.assertEqual(call2(o.sub, a, b), (a + 2**256 - b) % 2**256)

    @given(a=integers(min_value=min_value, max_value=max_value), b=integers(min_value=min_value, max_value=max_value))
    def test_mult(self, a, b):
        self.assertEqual(call2(o.mult, a, b), (a * b) % 2**256)

    @given(a=integers(min_value=min_value, max_value=max_value), b=integers(min_value=1, max_value=max_value))
    def test_div(self, a, b):
        self.assertEqual(call2(o.div, a, b), a // b)

    @given(a=integers(min_value=min_value, max_value=max_value), b=integers(min_value=min_value, max_value=max_value))
    def test_pow(self, a, b):
        self.assertEqual(call2(o.pow, a, b), (a ** b) % 2**256)

    @given(a=integers(min_value=min_value, max_value=max_value), b=integers(min_value=1, max_value=max_value))
    def test_mod(self, a, b):
        self.assertEqual(call2(o.mod, a, b), a % b)

    @given(a=integers(min_value=min_value, max_value=max_value), b=integers(min_value=min_value, max_value=256))
    def test_shl(self, a, b):
        self.assertEqual(call2(o.shl, a, b), (a << b) % 2**256)

    @given(a=integers(min_value=min_value, max_value=max_value), b=integers(min_value=min_value, max_value=256))
    def test_shr(self, a, b):
        self.assertEqual(call2(o.shr, a, b), (a >> b) % 2**256)

    @given(a=integers(min_value=min_value, max_value=max_value))
    def test_pack_unpack_roundtrip(self, a):
        self.assertEqual(call1(o.pack_unpack_roundtrip, a), a)

    @given(a=integers(min_value=min_value, max_value=max_value))
    def test_popcount(self, a):
        self.assertEqual(o.popcount(to_fut(encode(a))), bin(a).count('1'))

if __name__ == '__main__':
    print("entry points: ", list(o.entry_points.keys()))
    unittest.main()
