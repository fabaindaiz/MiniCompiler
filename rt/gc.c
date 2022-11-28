#include "lib.c"


/* configuration */
u64 STACK_SIZE = 0x800000;
u64 HEAP_SIZE = 48;
int USE_GC = 1;


/* GC */
u64* HEAP_START;
u64* HEAP_END;
u64* HEAP_MID;
u64* TO_SPACE;
u64* FROM_SPACE;
u64* ALLOC_PTR = 0;
u64* SCAN_PTR = 0;
u64* STACK_BOTTOM = 0;


void print_stack(u64* rbp, u64* rsp) {
  printf("|------- frame %p to %p  ------\n", rsp, rbp);
  for (u64* cur_word = rsp; cur_word < rbp; cur_word++) {
    u64 val = (u64)*cur_word;
    printf("| %p: %p", cur_word, (u64*)*cur_word);
    if (is_heap_ptr(val)) {
      if (is_tuple(val)){ printf(" (tuple)"); }
      else if (is_closure(val)){ printf(" (closure)"); }
    }
    printf("\n");
  }
  if (rbp < STACK_BOTTOM) {
    print_stack((u64*)*rbp, rbp + 2);
  }
  else {
    printf("|------- bottom %p  ------\n\n", STACK_BOTTOM);
  }
}

void print_heap(u64* heap_start, u64* heap_end){
  printf("| Heap from %p to %p\n", heap_start, heap_end);
  for (u64 i = 0; i <= (u64)(heap_end - heap_start); i++) {
    printf("|  %lud/%p: %p \n", i, (heap_start + i), (u64*)*(heap_start + i));
  }
}

void print_heaps(){
  printf("|\n|=======HEAP 1==========\n");
  print_heap(HEAP_START, HEAP_MID-1);
  printf("|=======HEAP 2==========\n");
  print_heap(HEAP_MID, HEAP_END);
  printf("|=================\n\n");
}


void set_stack_bottom(u64* stack_bottom) {
  STACK_BOTTOM = stack_bottom;
}

bool is_heap_ptr(u64 val){
  return (u64*)val < HEAP_END && (u64*)val >= HEAP_START;
}

// verifica que puntero este entre ciertos márgenes
bool is_valid_pointer(u64* reg){
  return HEAP_START <= reg && reg < HEAP_END; 
}


// copia los elementos a los que apunta val al nuevo espacio y deja un valor forward en el antiguo espacio
// retorna el objecto con su direccion (taggeada) en el nuevo espacio
u64 move(u64 val, u64 size, uint64_t tag){
  if ((((u64*)val)[0] & FORWARDER_TAG) == FORWARDER_TAG){
    return (u64) (((u64*)val)[0] - FORWARDER_TAG + tag);
  }

  u64 forward = (u64) ALLOC_PTR + FORWARDER_TAG;
  for (size_t i = 0; i < size; i++)
  {
    ALLOC_PTR[i] = ((u64*)(val ^ tag))[i];
  }
  u64 new_val = ((u64) ALLOC_PTR) + tag;
  ALLOC_PTR += size;
  ((u64*)val)[0] = forward;
  return new_val;
}

// revisa el contenido de reg y si es una referencia a un objeto lo copia al to_space, luego iterativamente 
// busca referencias a objetos entre los valores copiados y mueve esos objetos tambien
void scan_reg(u64* reg) {
  do {
    u64 val = *reg; 
    if (is_tuple(val) && is_valid_pointer((u64*)(val ^ TUPLE_TAG))){
      u64 size = *((u64*)(val ^ TUPLE_TAG));
      *reg = move(val, size+1, TUPLE_TAG);
      
    }
    else if(is_closure(val) && is_valid_pointer((u64*)(val ^ CLOSURE_TAG))){
      u64 size = ((u64*)(val ^ CLOSURE_TAG))[2];
      *reg = move(val, size+3, CLOSURE_TAG);
    }

    reg = ++SCAN_PTR;
  }
  while (ALLOC_PTR > SCAN_PTR);
}

u64* collect(u64* cur_frame, u64* cur_sp) {
  /* TBD: see https://en.wikipedia.org/wiki/Cheney%27s_algorithm */
  // swap from-space to-space
  u64* tmp = TO_SPACE;
  TO_SPACE = FROM_SPACE;
  FROM_SPACE = tmp;
  // init spaces 
  ALLOC_PTR = FROM_SPACE;
  SCAN_PTR = ALLOC_PTR;
  // scan stack and copy roots
  // scan objects in the heap
  while(true){
    for (u64 i = 0; i < (u64)(cur_frame - cur_sp); i++) {

      scan_reg(cur_sp + i);
    }
    
    cur_sp = cur_frame;
    if(cur_sp >= STACK_BOTTOM){
      break;
    }
    cur_frame = (u64*)(cur_sp[0]);
    cur_sp += 2;
  }
  // clean old space
  return ALLOC_PTR;
}


/* trigger GC if enabled and needed, out-of-memory error if insufficient */
u64* try_gc(u64* alloc_ptr, u64 words_needed, u64* cur_frame, u64* cur_sp) {
  if (USE_GC==1 && alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE) {
    printf("| need memory: GC!\n");
    alloc_ptr = collect(cur_frame, cur_sp);
  }
  if (alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE) {
    printf("| Error: out of memory!\n\n");
    print_stack(cur_frame, cur_sp);
    print_heaps();
    exit(-1);
  }
  return alloc_ptr;
}
