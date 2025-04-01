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
  //TODO
  return NULL;
}


int main() {
  //TODO
}
  
  
