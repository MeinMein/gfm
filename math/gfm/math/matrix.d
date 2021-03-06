module gfm.math.matrix;

import std.math,
       std.typetuple,
       std.traits,
       std.string,
       std.typecons,
       std.conv;

import gfm.math.vector,
       gfm.math.shapes,
       gfm.math.quaternion;

/// Generic non-resizeable matrix with R rows and C columns.
/// Intended for 3D use (size 3x3 and 4x4).
/// Important: <b>Matrices here are in row-major order whereas OpenGL is column-major.</b>
/// Params: 
///   T = type of elements
///   R = number of rows
///   C = number of columns
align(1) struct Matrix(T, size_t R, size_t C)
{
    align(1):
    public
    {
        static assert(R >= 1u && C >= 1u);

        alias Vector!(T, C) row_t;
        alias Vector!(T, R) column_t;

        enum bool isSquare = (R == C);

        // fields definition
        union
        {
            T[C*R] v;        // all elements
            row_t[R] rows;   // all rows
            T[C][R] c;       // components
        }

        this(U...)(U values) pure nothrow
        {
            static if ((U.length == C*R) && allSatisfy!(isTConvertible, U))
            {
                // construct with components
                foreach(int i, x; values)
                    v[i] = x;
            }
            else static if ((U.length == 1) && (isAssignable!(U[0])) && (!is(U[0] : Matrix)))
            {
                // construct with assignment
                opAssign!(U[0])(values[0]);
            }
            else static assert(false, "cannot create a matrix from given arguments");
        }

        /// Construct a matrix from columns.
        static Matrix fromColumns(column_t[] columns) pure nothrow
        {
            assert(columns.length == C);
            Matrix res;
            for (size_t i = 0; i < R; ++i)
                for (size_t j = 0; j < C; ++j)
                {
                   res.c[i][j] = columns[j][i];
                }
            return res;
        }

        /// Construct a matrix from rows.
        static Matrix fromRows(row_t[] rows) pure nothrow
        {
            assert(rows.length == R);
            Matrix res;
            res.rows[] = rows[];
            return res;
        }

        /// Construct matrix with a scalar.
        this(U)(T x)
        {
            for (size_t i = 0; i < _N; ++i)
                v[i] = x;
        }

        /// Assign with a samey matrice.
        ref Matrix opAssign(U : Matrix)(U x) pure nothrow
        {
            for (size_t i = 0; i < R * C; ++i)
                v[i] = x.v[i];
            return this;
        }

        /// Assign from other small matrices (same size, compatible type).
        ref Matrix opAssign(U)(U x) pure nothrow
            if (is(typeof(U._isMatrix))
                && is(U._T : _T)
                && (!is(U: Matrix))
                && (U._R == R) && (U._C == C))
        {
            for (size_t i = 0; i < R * C; ++i)
                v[i] = x.v[i];
            return this;
        }

        /// Assign with a static array of size R * C.
        ref Matrix opAssign(U)(U x) pure nothrow
            if ((isStaticArray!U)
                && is(typeof(x[0]) : T)
                && (U.length == R * C))
        {
            for (size_t i = 0; i < R * C; ++i)
                v[i] = x[i];
            return this;
        }

        /// Assign with a dynamic array of size R * C.
        ref Matrix opAssign(U)(U x) pure nothrow
            if ((isDynamicArray!U)
                && is(typeof(x[0]) : T))
        {
            assert(x.length == R * C);
            for (size_t i = 0; i < R * C; ++i)
                v[i] = x[i];
            return this;
        }

        /// Return a pointer to content.
        T* ptr() pure nothrow @property
        {
            return v.ptr;
        }

        /// Returns: column j as a vector.
        column_t column(size_t j) pure const nothrow
        {
            column_t res = void;
            for (size_t i = 0; i < R; ++i)
                res.v[i] = c[i][j];
            return res;
        }

        /// Returns: row i as a vector.
        row_t row(size_t i) pure const nothrow
        {
            return rows[i];
        }

        /// Covnerts to pretty string.
        string toString() const nothrow
        {
            try
                return format("%s", v);
            catch (Exception e) 
                assert(false); // should not happen since format is right
        }

        /// Matrix * vector multiplication.
        column_t opBinary(string op)(row_t x) pure const nothrow if (op == "*")
        {
            column_t res = void;
            for (size_t i = 0; i < R; ++i)
            {
                T sum = 0;
                for (size_t j = 0; j < C; ++j)
                {
                    sum += c[i][j] * x.v[j];
                }
                res.v[i] = sum;
            }
            return res;
        }

        /// Matrix * matrix multiplication.
        auto opBinary(string op, U)(U x) pure const nothrow
            if (is(typeof(U._isMatrix)) && (U._R == C) && (op == "*"))
        {
            Matrix!(T, R, U._C) result = void;

            for (size_t i = 0; i < R; ++i)
            {
                for (size_t j = 0; j < U._C; ++j)
                {
                    T sum = 0;
                    for (size_t k = 0; k < C; ++k)
                        sum += c[i][k] * x.c[k][j];
                    result.c[i][j] = sum;
                }
            }
            return result;
        }

        ref Matrix opOpAssign(string op, U)(U operand) pure nothrow if (isConvertible!U)
        {
            Matrix conv = operand;
            return opOpAssign!op(conv);
        }

        /// Cast to other matrix types.
        /// If the size are different, the result matrix is truncated 
        /// and/or filled with identity coefficients.
        U opCast(U)() pure nothrow const if (is(typeof(U._isMatrix)))
        {
            U res = U.identity();
            enum minR = R < U._R ? R : U._R;
            enum minC = C < U._C ? C : U._C;
            for (size_t i = 0; i < minR; ++i)
                for (size_t j = 0; j < minC; ++j)
                {
                    res.c[i][j] = cast(U._T)(c[i][j]);
                }
            return res;
        }

        bool opEquals(U)(U other) pure const nothrow if (is(U : Matrix))
        {
            for (size_t i = 0; i < R * C; ++i)
                if (v[i] != other.v[i])
                    return false;
            return true;
        }

        bool opEquals(U)(U other) pure const nothrow
            if ((isAssignable!U) && (!is(U: Matrix)))
        {
            Matrix conv = other;
            return opEquals(conv);
        }

        // +matrix, -matrix, ~matrix, !matrix
        Matrix opUnary(string op)() pure const nothrow if (op == "+" || op == "-" || op == "~" || op == "!")
        {
            Matrix res = void;
            for (size_t i = 0; i < N; ++i)
                mixin("res.v[i] = " ~ op ~ "v[i];");
            return res;
        }

        /// Convert 3x3 rotation matrix to quaternion.
        /// See_also: 3D Math Primer for Graphics and Game Development.
        U opCast(U)() pure const nothrow if (is(typeof(U._isQuaternion))
                                          && is(U._T : _T)
                                          && (_R == 3) && (_C == 3))
        {
            T fourXSquaredMinus1 = c[0][0] - c[1][1] - c[2][2];
            T fourYSquaredMinus1 = c[1][1] - c[0][0] - c[2][2];
            T fourZSquaredMinus1 = c[2][2] - c[0][0] - c[1][1];
            T fourWSquaredMinus1 = c[0][0] + c[1][1] + c[2][2];

            int biggestIndex = 0;
            T fourBiggestSquaredMinus1 = fourWSquaredMinus1;

            if(fourXSquaredMinus1 > fourBiggestSquaredMinus1)
            {
                fourBiggestSquaredMinus1 = fourXSquaredMinus1;
                biggestIndex = 1;
            }

            if(fourYSquaredMinus1 > fourBiggestSquaredMinus1)
            {
                fourBiggestSquaredMinus1 = fourYSquaredMinus1;
                biggestIndex = 2;
            }

            if(fourZSquaredMinus1 > fourBiggestSquaredMinus1)
            {
                fourBiggestSquaredMinus1 = fourZSquaredMinus1;
                biggestIndex = 3;
            }

            T biggestVal = sqrt(fourBiggestSquaredMinus1 + 1) / 2;
            T mult = 1 / (biggestVal * 4);

            U quat;
            switch(biggestIndex)
            {
                case 1:
                    quat.w = (c[1][2] - c[2][1]) * mult;
                    quat.x = biggestVal;
                    quat.y = (c[0][1] + c[1][0]) * mult;
                    quat.z = (c[2][0] + c[0][2]) * mult;
                    break;

                case 2:
                    quat.w = (c[2][0] - c[0][2]) * mult;
                    quat.x = (c[0][1] + c[1][0]) * mult;
                    quat.y = biggestVal;
                    quat.z = (c[1][2] + c[2][1]) * mult;
                    break;

                case 3:
                    quat.w = (c[0][1] - c[1][0]) * mult;
                    quat.x = (c[2][0] + c[0][2]) * mult;
                    quat.y = (c[1][2] + c[2][1]) * mult;
                    quat.z = biggestVal;
                    break;

                default: // biggestIndex == 0
                    quat.w = biggestVal; 
                    quat.x = (c[1][2] - c[2][1]) * mult;
                    quat.y = (c[2][0] - c[0][2]) * mult;
                    quat.z = (c[0][1] - c[1][0]) * mult;
                    break;
            }

            return quat;
        }

        /// Converts a 4x4 rotation matrix to quaternion.
        U opCast(U)() pure const nothrow if (is(typeof(U._isQuaternion))
                                          && is(U._T : _T)
                                          && (_R == 4) && (_C == 4))
        {
            auto m3 = cast(mat3!T)(this);
            return cast(U)(m3);
        }

        /// Matrix inversion is provided for 2x2, 3x3 and 4x4 floating point matrices.

        static if (isSquare && isFloatingPoint!T && R == 2)
        {
            /// Returns: inverse of matrix.
            Matrix inverse() pure const nothrow
            {
                T invDet = 1 / (c[0][0] * c[1][1] - c[0][1] * c[1][0]);
                return Matrix( c[1][1] * invDet, -c[0][1] * invDet,
                                   -c[1][0] * invDet,  c[0][0] * invDet);
            }
        }

        static if (isSquare && isFloatingPoint!T && R == 3)
        {
            /// Returns: inverse of matrix.
            Matrix inverse() pure const nothrow
            {
                T det = c[0][0] * (c[1][1] * c[2][2] - c[2][1] * c[1][2])
                      - c[0][1] * (c[1][0] * c[2][2] - c[1][2] * c[2][0])
                      + c[0][2] * (c[1][0] * c[2][1] - c[1][1] * c[2][0]);
                T invDet = 1 / det;

                Matrix res = void;
                res.c[0][0] =  (c[1][1] * c[2][2] - c[2][1] * c[1][2]) * invDet;
                res.c[0][1] = -(c[0][1] * c[2][2] - c[0][2] * c[2][1]) * invDet;
                res.c[0][2] =  (c[0][1] * c[1][2] - c[0][2] * c[1][1]) * invDet;
                res.c[1][0] = -(c[1][0] * c[2][2] - c[1][2] * c[2][0]) * invDet;
                res.c[1][1] =  (c[0][0] * c[2][2] - c[0][2] * c[2][0]) * invDet;
                res.c[1][2] = -(c[0][0] * c[1][2] - c[1][0] * c[0][2]) * invDet;
                res.c[2][0] =  (c[1][0] * c[2][1] - c[2][0] * c[1][1]) * invDet;
                res.c[2][1] = -(c[0][0] * c[2][1] - c[2][0] * c[0][1]) * invDet;
                res.c[2][2] =  (c[0][0] * c[1][1] - c[1][0] * c[0][1]) * invDet;
                return res;
            }
        }

        static if (isSquare && isFloatingPoint!T && R == 4)
        {
            /// Returns: inverse of matrix.
            Matrix inverse() pure const nothrow
            {
                T det2_01_01 = c[0][0] * c[1][1] - c[0][1] * c[1][0];
                T det2_01_02 = c[0][0] * c[1][2] - c[0][2] * c[1][0];
                T det2_01_03 = c[0][0] * c[1][3] - c[0][3] * c[1][0];
                T det2_01_12 = c[0][1] * c[1][2] - c[0][2] * c[1][1];
                T det2_01_13 = c[0][1] * c[1][3] - c[0][3] * c[1][1];
                T det2_01_23 = c[0][2] * c[1][3] - c[0][3] * c[1][2];

                T det3_201_012 = c[2][0] * det2_01_12 - c[2][1] * det2_01_02 + c[2][2] * det2_01_01;
                T det3_201_013 = c[2][0] * det2_01_13 - c[2][1] * det2_01_03 + c[2][3] * det2_01_01;
                T det3_201_023 = c[2][0] * det2_01_23 - c[2][2] * det2_01_03 + c[2][3] * det2_01_02;
                T det3_201_123 = c[2][1] * det2_01_23 - c[2][2] * det2_01_13 + c[2][3] * det2_01_12;

                T det = - det3_201_123 * c[3][0] + det3_201_023 * c[3][1] - det3_201_013 * c[3][2] + det3_201_012 * c[3][3];
                T invDet = 1 / det;

                T det2_03_01 = c[0][0] * c[3][1] - c[0][1] * c[3][0];
                T det2_03_02 = c[0][0] * c[3][2] - c[0][2] * c[3][0];
                T det2_03_03 = c[0][0] * c[3][3] - c[0][3] * c[3][0];
                T det2_03_12 = c[0][1] * c[3][2] - c[0][2] * c[3][1];
                T det2_03_13 = c[0][1] * c[3][3] - c[0][3] * c[3][1];
                T det2_03_23 = c[0][2] * c[3][3] - c[0][3] * c[3][2];
                T det2_13_01 = c[1][0] * c[3][1] - c[1][1] * c[3][0];
                T det2_13_02 = c[1][0] * c[3][2] - c[1][2] * c[3][0];
                T det2_13_03 = c[1][0] * c[3][3] - c[1][3] * c[3][0];
                T det2_13_12 = c[1][1] * c[3][2] - c[1][2] * c[3][1];
                T det2_13_13 = c[1][1] * c[3][3] - c[1][3] * c[3][1];
                T det2_13_23 = c[1][2] * c[3][3] - c[1][3] * c[3][2];

                T det3_203_012 = c[2][0] * det2_03_12 - c[2][1] * det2_03_02 + c[2][2] * det2_03_01;
                T det3_203_013 = c[2][0] * det2_03_13 - c[2][1] * det2_03_03 + c[2][3] * det2_03_01;
                T det3_203_023 = c[2][0] * det2_03_23 - c[2][2] * det2_03_03 + c[2][3] * det2_03_02;
                T det3_203_123 = c[2][1] * det2_03_23 - c[2][2] * det2_03_13 + c[2][3] * det2_03_12;

                T det3_213_012 = c[2][0] * det2_13_12 - c[2][1] * det2_13_02 + c[2][2] * det2_13_01;
                T det3_213_013 = c[2][0] * det2_13_13 - c[2][1] * det2_13_03 + c[2][3] * det2_13_01;
                T det3_213_023 = c[2][0] * det2_13_23 - c[2][2] * det2_13_03 + c[2][3] * det2_13_02;
                T det3_213_123 = c[2][1] * det2_13_23 - c[2][2] * det2_13_13 + c[2][3] * det2_13_12;

                T det3_301_012 = c[3][0] * det2_01_12 - c[3][1] * det2_01_02 + c[3][2] * det2_01_01;
                T det3_301_013 = c[3][0] * det2_01_13 - c[3][1] * det2_01_03 + c[3][3] * det2_01_01;
                T det3_301_023 = c[3][0] * det2_01_23 - c[3][2] * det2_01_03 + c[3][3] * det2_01_02;
                T det3_301_123 = c[3][1] * det2_01_23 - c[3][2] * det2_01_13 + c[3][3] * det2_01_12;

                Matrix res = void;
                res.c[0][0] = - det3_213_123 * invDet;
                res.c[1][0] = + det3_213_023 * invDet;
                res.c[2][0] = - det3_213_013 * invDet;
                res.c[3][0] = + det3_213_012 * invDet;

                res.c[0][1] = + det3_203_123 * invDet;
                res.c[1][1] = - det3_203_023 * invDet;
                res.c[2][1] = + det3_203_013 * invDet;
                res.c[3][1] = - det3_203_012 * invDet;

                res.c[0][2] = + det3_301_123 * invDet;
                res.c[1][2] = - det3_301_023 * invDet;
                res.c[2][2] = + det3_301_013 * invDet;
                res.c[3][2] = - det3_301_012 * invDet;

                res.c[0][3] = - det3_201_123 * invDet;
                res.c[1][3] = + det3_201_023 * invDet;
                res.c[2][3] = - det3_201_013 * invDet;
                res.c[3][3] = + det3_201_012 * invDet;
                return res;
            }
        }

        /// Returns: transposed matrice.
        Matrix!(T, C, R) transposed() pure const nothrow
        {
            Matrix!(T, C, R) res;
            for (size_t i = 0; i < C; ++i)
                for (size_t j = 0; j < R; ++j)
                    res.c[i][j] = c[j][i];
            return res;
        }

        static if (isSquare && R > 1)
        {
            /// In-place translate by (v, 1)
            void translate(Vector!(T, R-1) v) pure nothrow
            {
                for (size_t i = 0; i < R; ++i)
                {
                    T dot = 0;
                    for (size_t j = 0; j + 1 < C; ++j)
                        dot += v.v[j] * c[i][j];

                    c[i][C-1] += dot;
                }
            }

            /// Make a translation matrix.
            static Matrix translation(Vector!(T, R-1) v) pure nothrow
            {
                Matrix res = identity();
                for (size_t i = 0; i + 1 < R; ++i)
                    res.c[i][C-1] += v.v[i];
                return res;
            }

            /// In-place matrix scaling.
            void scale(Vector!(T, R-1) v) pure nothrow
            {
                for (size_t i = 0; i < R; ++i)
                    for (size_t j = 0; j + 1 < C; ++j)
                        c[i][j] *= v.v[j];
            }

            /// Make a scaling matrix.
            static Matrix scaling(Vector!(T, R-1) v) pure nothrow
            {
                Matrix res = identity();
                for (size_t i = 0; i + 1 < R; ++i)
                    res.c[i][i] = v.v[i];
                return res;
            }
        }

        // rotations are implemented for 3x3 and 4x4 matrices.
        static if (isSquare && (R == 3 || R == 4) && isFloatingPoint!T)
        {
            private static Matrix rotateAxis(size_t i, size_t j)(T angle) pure nothrow
            {
                Matrix res = identity();
                const T cosa = cos(angle);
                const T sina = sin(angle);
                res.c[i][i] = cosa;
                res.c[i][j] = -sina;
                res.c[j][i] = sina;
                res.c[j][j] = cosa;
                return res;
            }

            /// Returns: rotation matrix along axis X
            public alias rotateAxis!(1, 2) rotateX;

            /// Returns: rotation matrix along axis Y
            public alias rotateAxis!(2, 0) rotateY;

            /// Returns: rotation matrix along axis Z
            public alias rotateAxis!(0, 1) rotateZ;

            /// Similar to the glRotate matrix, however the angle is expressed in radians
            /// See_also: $(LINK http://www.cs.rutgers.edu/~decarlo/428/gl_man/rotate.html)
            static Matrix rotation(T angle, vec3!T axis) pure nothrow
            {
                Matrix res = identity();
                const T c = cos(angle);
                const oneMinusC = 1 - c;
                const T s = sin(angle);
                axis = axis.normalized();
                T x = axis.x,
                  y = axis.y,
                  z = axis.z;
                T xy = x * y,
                  yz = y * z,
                  xz = x * z;

                res.c[0][0] = x * x * oneMinusC + c;
                res.c[0][1] = x * y * oneMinusC - z * s;
                res.c[0][2] = x * z * oneMinusC + y * s;
                res.c[1][0] = y * x * oneMinusC + z * s;
                res.c[1][1] = y * y * oneMinusC + c;
                res.c[1][2] = y * z * oneMinusC - x * s;
                res.c[2][0] = z * x * oneMinusC - y * s;
                res.c[2][1] = z * y * oneMinusC + x * s;
                res.c[2][2] = z * z * oneMinusC + c;
                return res;
            }
        }

        // 4x4 specific transformations for 3D usage
        static if (isSquare && R == 4 && isFloatingPoint!T)
        {
            /// Returns: orthographic projection.
            static Matrix orthographic(T left, T right, T bottom, T top, T near, T far) pure nothrow
            {
                T dx = right - left,
                  dy = top - bottom,
                  dz = far - near;

                T tx = -(right + left) / dx;
                T ty = -(top + bottom) / dy;
                T tz = -(far + near)   / dz;

                return Matrix(2 / dx,   0,      0,    tx,
                                0,    2 / dy,   0,    ty,
                                0,      0,    2 / dz, tz,
                                0,      0,      0,     1);
            }

            /// Returns: perspective projection.
            static Matrix perspective(T FOVInRadians, T aspect, T zNear, T zFar) pure nothrow
            {
                T f = 1 / tan(FOVInRadians / 2);
                T d = 1 / (zNear - zFar);

                return Matrix(f / aspect, 0,                  0,                    0,
                                       0, f,                  0,                    0,
                                       0, 0, (zFar + zNear) * d, 2 * d * zFar * zNear,
                                       0, 0,                 -1,                    0);
            }

            /// Returns: "lookAt" projection.
            /// Thanks to vuaru for corrections.
            static Matrix lookAt(vec3!T eye, vec3!T target, vec3!T up) pure nothrow
            {
                vec3!T Z = (eye - target).normalized();
                vec3!T X = cross(-up, Z).normalized();
                vec3!T Y = cross(Z, -X);

                return Matrix(-X.x,        -X.y,        -X.z,      dot(X, eye),
                               Y.x,         Y.y,         Y.z,     -dot(Y, eye),
                               Z.x,         Z.y,         Z.z,     -dot(Z, eye),
                               0,           0,           0,        1);
            }

            /// Extract frustum from a 4x4 matrice.
            Frustum!T frustum() pure const nothrow
            {
                auto left   = Plane!T(row(3) + row(0));
                auto right  = Plane!T(row(3) - row(0));
                auto top    = Plane!T(row(3) - row(1));
                auto bottom = Plane!T(row(3) + row(1));
                auto near   = Plane!T(row(3) + row(2));
                auto far    = Plane!T(row(3) - row(2));
                return Frustum!T(left, right, top, bottom, near, far);
            }

        }
    }

    private
    {
        alias T _T;
        enum _R = R;
        enum _C = C;
        enum bool _isMatrix = true;

        template isAssignable(T)
        {
            enum bool isAssignable =
                is(typeof(
                {
                    T x;
                    Matrix m = x;
                }()));
        }

        template isTConvertible(U)
        {
            enum bool isTConvertible = is(U : T);
        }

        template isRowConvertible(U)
        {
            enum bool isRowConvertible = is(U : row_t);
        }

        template isColumnConvertible(U)
        {
            enum bool isColumnConvertible = is(U : column_t);
        }
    }

    public
    {
        /// Returns: an identity matrice.
        /// Note: the identity matrix, while only meaningful for square matrices, 
        /// is also defined for non-square ones.
        static Matrix identity() pure nothrow
        {
            Matrix res = void;
            for (size_t i = 0; i < R; ++i)
                for (size_t j = 0; j < C; ++j)
                    res.c[i][j] = (i == j) ? 1 : 0;
            return res;
        }

        /// Returns: a constant matrice.
        static Matrix constant(U)(U x) pure nothrow
        {
            Matrix res = void;
            
            for (size_t i = 0; i < R * C; ++i)
                res.v[i] = cast(T)x;
            return res;
        }
    }
}

// GLSL is a big inspiration here
// we defines types with more or less the same names
template mat2x2(T) { alias Matrix!(T, 2u, 2u) mat2x2; }
template mat3x3(T) { alias Matrix!(T, 3u, 3u) mat3x3; }
template mat4x4(T) { alias Matrix!(T, 4u, 4u) mat4x4; }

// WARNING: in GLSL, first number is _columns_, second is rows
// It is the opposite here: first number is rows, second is columns
// With this convention mat2x3 * mat3x4 -> mat2x4.
template mat2x3(T) { alias Matrix!(T, 2u, 3u) mat2x3; }
template mat2x4(T) { alias Matrix!(T, 2u, 4u) mat2x4; }
template mat3x2(T) { alias Matrix!(T, 3u, 2u) mat3x2; }
template mat3x4(T) { alias Matrix!(T, 3u, 4u) mat3x4; }
template mat4x2(T) { alias Matrix!(T, 4u, 2u) mat4x2; }
template mat4x3(T) { alias Matrix!(T, 4u, 3u) mat4x3; }

alias mat2x2 mat2;
alias mat3x3 mat3;  // shorter names for most common matrices
alias mat4x4 mat4;

private string definePostfixAliases(string type)
{
    return "alias " ~ type ~ "!byte "   ~ type ~ "b;\n"
         ~ "alias " ~ type ~ "!ubyte "  ~ type ~ "ub;\n"
         ~ "alias " ~ type ~ "!short "  ~ type ~ "s;\n"
         ~ "alias " ~ type ~ "!ushort " ~ type ~ "us;\n"
         ~ "alias " ~ type ~ "!int "    ~ type ~ "i;\n"
         ~ "alias " ~ type ~ "!uint "   ~ type ~ "ui;\n"
         ~ "alias " ~ type ~ "!long "   ~ type ~ "l;\n"
         ~ "alias " ~ type ~ "!ulong "  ~ type ~ "ul;\n"
         ~ "alias " ~ type ~ "!float "  ~ type ~ "f;\n"
         ~ "alias " ~ type ~ "!double " ~ type ~ "d;\n"
         ~ "alias " ~ type ~ "!real "   ~ type ~ "L;\n";
}

// define a lot of type names
mixin(definePostfixAliases("mat2"));
mixin(definePostfixAliases("mat3"));
mixin(definePostfixAliases("mat4"));
mixin(definePostfixAliases("mat2x2"));
mixin(definePostfixAliases("mat2x3"));
mixin(definePostfixAliases("mat2x4"));
mixin(definePostfixAliases("mat3x2"));
mixin(definePostfixAliases("mat3x3"));
mixin(definePostfixAliases("mat3x4"));
mixin(definePostfixAliases("mat4x2"));
mixin(definePostfixAliases("mat4x3"));
mixin(definePostfixAliases("mat4x4"));

unittest
{
    mat2i x = mat2i(0, 1,
                    2, 3);
    assert(x.c[0][0] == 0 && x.c[0][1] == 1 && x.c[1][0] == 2 && x.c[1][1] == 3);

    vec2i[2] cols = [vec2i(0, 2), vec2i(1, 3)];
    mat2i y = mat2i.fromColumns(cols[]);
    assert(y.c[0][0] == 0 && y.c[0][1] == 1 && y.c[1][0] == 2 && y.c[1][1] == 3);
    y = mat2i.fromRows(cols[]);
    assert(y.c[0][0] == 0 && y.c[1][0] == 1 && y.c[0][1] == 2 && y.c[1][1] == 3);
    y = y.transposed();

    assert(x == y);
    x = [0, 1, 2, 3];
    assert(x == y);


    mat2i z = x * y;
    assert(z == mat2i([2, 3, 6, 11]));
    vec2i vz = z * vec2i(2, -1);
    assert(vz == vec2i(1, 1));

    mat2f a = z;
    mat2f w = [4, 5, 6, 7];
    z = cast(mat2i)w;
    assert(w == z);

    {
        mat2x3f A;
        mat3x4f B;
        mat2x4f C = A * B;
    }
}
