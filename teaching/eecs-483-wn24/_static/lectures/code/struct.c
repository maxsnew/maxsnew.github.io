#include "stdio.h"
#include <stdint.h>

struct Point {int64_t x; int64_t y;};
struct Rect {struct Point ll, lr, ul, ur; };

struct Rect mk_square(struct Point ll, int64_t len) {
  ll.x = 3;
  struct Rect square;
  square.ll = square.lr = square.ul = square.ur = ll;
  square.lr.x += len;
  square.ul.y += len;
  square.ur.x += len;
  square.ur.y += len;
  return square;
}

void mk_square2(struct Point *ll, int64_t elen,
              struct Rect *res) {
  res->lr = res->ul = res->ur = res->ll = *ll;
  res->lr.x += elen;
  res->ur.x += elen; 
  res->ur.y += elen;
  res->ul.y += elen;
}

void print_rect(struct Rect r) {
  printf("RECT: = ll(%llu, %llu), lr(%llu, %llu), ul(%llu, %llu), ur(%llu, %llu)\n",
	 r.ll.x, r.ll.y, r.lr.x, r.lr.y, r.ul.x, r.ul.y, r.ur.x, r.ur.y);
}

void foo() {
  struct Point origin = {0,0};
  struct Rect unit_sq;
  mk_square2(&origin, 1, &unit_sq);
  printf("mk_square2\n");
  print_rect(unit_sq);
}


int main(void) {
  struct Point ll;
  ll.x = 0;
  ll.y = 0;

  printf("sizeof(int64_t) = %lu\n", sizeof(int64_t));
  printf("sizeof(struct Point) = %lu\n", sizeof(struct Point));
  printf("size: %lu\n", sizeof(struct Point) + sizeof(struct Point) + sizeof(int64_t));

  printf("mk_square\n");
  struct Rect s = mk_square(ll, 3);
  print_rect(s);

  foo();

  return 0;
}
