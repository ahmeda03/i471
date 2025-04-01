#include <iostream>

class C {
public:
  C(const char *addrType, int val) : addrType(addrType), val(val) {
    std::cerr << "alloc (" << addrType << ", " << val << ")\n";
  }
  ~C() { std::cerr << "free (" << addrType << ", " << val << ")\n"; }

private:
  const char *addrType;
  int val;  
};

/* Set up calls to C in main() and f1() to produce output:
   
alloc (stack, 11)
alloc (static, -20)
alloc (heap, 13)
free (stack, 11)
free (heap, 13)
free (static, -20)

*/


static C *f1(int n) { 
  C c1("stack", 10*n + 1);
  static C c2("static", -20);
  C *c3 = new C("heap", 10*n + 3);
  return c3;
}


int main() {
  C *c = f1(1);
  delete c;
}
  
  
