extern register< T > { void write(in bit< 32 > index, in T value); }
void f< T >(register< T > r, in T t) { r.write(0, t); }
