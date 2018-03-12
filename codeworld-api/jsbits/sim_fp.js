/*
 * Copyright 2018 The CodeWorld Authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// Replaces JavaScript's Math module with deterministic implementations
// of all transcendental and other potentially under-specified values.
// The intent of this is to make it safer to run distributed
// computations and rely on getting the same result in all clients.
//
// This may (and likely will) make some floating point operations less
// accurate!
function cw$deterministic_math() {
  Math.E = 2.718281828459045;
  Math.LN10 = 2.302585092994046;
  Math.LN2 = 0.6931471805599453;
  Math.LOG10E = 0.4342944819032518;
  Math.LOG2E = 1.4426950408889634;
  Math.PI = 3.141592653589793;
  Math.SQRT1_2 = 0.7071067811865476;
  Math.SQRT2 = 1.4142135623730951;

  Math.cbrt = function(x) {
    return Math.pow(x, 1.0 / 3.0);
  }

  Math.exp = function(x) {
    if (x < 2) {
      // Compute using the power series.
      var term = 1;
      var sum = 1;
      for (var i = 1; term > 1e-10; ++i) {
        term *= x / i;
        sum += term;
      }
      return sum;
    }

    var sqrt = Math.exp(x/2);
    return sqrt * sqrt;
  }

  Math.expm1 = function(x) {
    return Math.exp(x) - 1;
  }

  Math.asin = function(x) {
    if (x < 0) return -Math.asin(-x);

    // Accurate to about 5 decimal places.  Should use a better approximation.
    var a0 =  1.5707288;
    var a1 = -0.2121144;
    var a2 =  0.0742610;
    var a3 = -0.0187293;

    var x2 = x * x;
    var x3 = x2 * x;

    return Math.PI / 2 - Math.sqrt(1 - x) * (a0 + a1 * x + a2 * x2 + a3 * x3);
  }

  Math.acos = function(x) {
    return Math.PI / 2 - Math.asin(x);
  }

  Math.atan = function(x) {
    if (Math.abs(x) > 1) return Math.sign(x) * Math.PI / 2 - Math.atan(1 / x);

    // Accurate to about 5 decimal places.  Should use a better approximation.
    var a1 =  0.9998660;
    var a3 = -0.3302995;
    var a5 =  0.1801410;
    var a7 = -0.0851330;
    var a9 =  0.0208351;

    var x2 = x  * x;
    var x3 = x  * x2;
    var x5 = x3 * x2;
    var x7 = x5 * x2;
    var x9 = x7 * x2;

    return a1 * x + a3 * x3 + a5 * x5 + a7 * x7 + a9 * x9;
  }

  Math.atan2 = function(y, x) {
    if (x == 0 && y > 0) return Math.PI / 2;
    if (x == 0 && y < 0) return -Math.PI / 2;
    var atan = Math.atan(y / x);
    if (x > 0) return atan;
    if (atan > 0) return atan - Math.PI;
    return atan + Math.PI;
  }

  Math.asinh = function(x) {
    return Math.log(x + Math.sqrt(x * x + 1));
  }

  Math.acosh = function(x) {
    return Math.log(x + Math.sqrt(x * x - 1));
  }

  Math.atanh = function(x) {
    return Math.log((1 + x) / (1 - x)) / 2;
  }

  Math.cos = function(x) {
    return Math.sin(x + Math.PI / 2);
  }

  Math.cosh = function(x) {
    return (Math.exp(z) + Math.exp(-z)) / 2;
  }

  Math.hypot = function() {
    var sumsq = 0;
    for (var i = 0; i < arguments.length; i++) {
      var x = arguments[i];
      sumsq += x * x;
    }
    return Math.sqrt(sumsq);
  }

  Math.log = function(x) {
    var float = new Float64Array(1);
    var bytes = new Uint8Array(float.buffer);
    float[0] = x;

    var exponent = ((bytes[7] & 0x7f) << 4 | bytes[6] >> 4) - 0x3ff;

    bytes[7] = 0x3f;
    bytes[6] |= 0xf0;
    var mantissa = float[0];

    var lg2;
    var a = 4.418508;
    var b = 9.143698;
    var c = 6.232189;
    var d = 6.337977;
    if (mantissa > 1.5) {
      var k = mantissa / 2 - 1;
      lg2 = exponent + 1 + (a * k * k + b * k) / (k * k + c * k + d);
    } else {
      var k = mantissa - 1;
      lg2 = exponent + (a * k * k + b * k) / (k * k + c * k + d);
    }
    return lg2 / Math.LOG2E;
  }

  Math.log1p = function(x) {
    return Math.log(1 + x);
  }

  Math.log10 = function(x) {
    return Math.log(x) / Math.LN10;
  }

  Math.log2 = function(x) {
    return Math.log(x) / Math.LN2;
  }

  Math.pow = function(base, exponent) {
    if (exponent < 0) {
      return 1 / Math.pow(base, -exponent);
    } else if (Math.floor(exponent) == exponent && exponent < 100) {
      // Exact implementation for small integer powers.
      function ipow(n) {
        if (n == 0) return 1;
        if (n % 2 == 1) return base * ipow(n - 1);
        var sqrt = ipow(n / 2);
        return sqrt * sqrt;
      }
      return ipow(exponent);
    } else if (base == 0) {
      return 0;
    } else {
      return Math.exp(exponent * Math.log(base));
    }
  }

  Math.sin = function(x) {
    if (x < -Math.PI) return Math.sin(x + 2 * Math.PI);
    if (x >  Math.PI) return Math.sin(x - 2 * Math.PI);
    var t = 1.27323954 * x - 0.405284735 * x * Math.abs(x);
    return 0.225 * t * (Math.abs(t) - 1) + t;
  }

  Math.sinh = function(x) {
    return (Math.exp(z) - Math.exp(-z)) / 2;
  }

  Math.tan = function(x) {
    return Math.sin(x) / Math.cos(x);
  }

  Math.tanh = function(x) {
    return Math.sinh(x) / Math.cosh(x);
  }
}
