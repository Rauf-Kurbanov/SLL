#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "runtime.h"

#define SLL_BLOCK_SIZE 2520
#define SLL_MAX_CTR_NAME_LEN 63

enum GCColor {
  SllWhite = 0,
  SllBlack = 1
};

enum Lexeme {
  SllEof     = -1,
  SllCtrName = -2
};

struct Block {
  struct Block *next;
  Word mem[SLL_BLOCK_SIZE];
};

struct ExternalCtrNamesTable {
  char const **hash_table;
  size_t mask;
  size_t size;
};

struct RootsBlock *sll_roots;
Word *sll_free_cell[SLL_MAX_OBJECT_SIZE];

struct Block *sll_heap[SLL_MAX_OBJECT_SIZE];

static size_t heap_size;
static int next_lexeme;
static char ctr_name_buf[SLL_MAX_CTR_NAME_LEN + 1];
static struct ExternalCtrNamesTable ext_ctr_names;

static void init_ext_ctr_names(size_t const capacity_mask) {
  ext_ctr_names.mask = capacity_mask;
  size_t const capacity_in_bytes = sizeof(char *) * (ext_ctr_names.mask + 1);
  ext_ctr_names.hash_table = (char const **)malloc(capacity_in_bytes);
  memset(ext_ctr_names.hash_table, 0, capacity_in_bytes);
}

static void dispose_ext_ctr_names() {
  for (size_t i = 0; i <= ext_ctr_names.mask; ++i)
    free((void *)ext_ctr_names.hash_table[i]);
  free(ext_ctr_names.hash_table);
}

static inline size_t str_hash(char const *str) {
  size_t acc = 0;
  while (*str)
    acc = (acc + *str++) * 241;
  return acc;
}

static void rehash_ext_ctr_names() {
  size_t const old_mask = ext_ctr_names.mask;
  char const **old_table = ext_ctr_names.hash_table;
  init_ext_ctr_names((old_mask << 1) | 1);
  for (size_t j = 0; j <= old_mask; ++j)
    if (old_table[j]) {
      size_t i = str_hash(old_table[j]) & ext_ctr_names.mask;
      while (ext_ctr_names.hash_table[i])
        i = (i + 1) & ext_ctr_names.mask;
      ext_ctr_names.hash_table[i] = old_table[j];
    }
  free(old_table);
}

static char const *get_ext_ctr_name() {
  for (size_t i = str_hash(ctr_name_buf);; ++i) {
    i &= ext_ctr_names.mask;
    char const *const curr_str = ext_ctr_names.hash_table[i];
    if (!curr_str) {
      char *const new_str = (char *)malloc(strlen(ctr_name_buf) + 1);
      strcpy(new_str, ctr_name_buf);
      ext_ctr_names.hash_table[i] = new_str;
      if (++ext_ctr_names.size > ext_ctr_names.mask / 4)
        rehash_ext_ctr_names();
      return new_str;
    }
    if (strcmp(curr_str, ctr_name_buf) == 0)
      return curr_str;
  }
}

static inline void gc_dfs(Word *const cell) {
  if (SLL_get_color(cell[0]) == SllBlack)
    return;
  cell[0] = SLL_set_color(cell[0], SllBlack);
  size_t const size = SLL_get_osize(cell[0]);
  for (size_t i = 1; i <= size; ++i)
    gc_dfs((Word *)cell[i]);
}

static void gc_mark() {
  for (struct RootsBlock *block = sll_roots; block; block = block->next) {
    size_t const size = block->size;
    Object *objects = (Object *)(block + 1);
    for (size_t i = 0; i < size; ++i)
      if (objects[i])
        gc_dfs((Word *)objects[i]);
  }
}

static size_t gc_sweep() {
  size_t live_heap_size = 0;
  for (size_t object_size = 0; object_size < SLL_MAX_OBJECT_SIZE; ++object_size) {
    sll_free_cell[object_size] = NULL;
    size_t const size_in_words = object_size + 1;
    size_t const block_size = (SLL_BLOCK_SIZE / size_in_words) * size_in_words;
    for (struct Block *block = sll_heap[object_size]; block; block = block->next)
      for (size_t i = 0; i < block_size; i += size_in_words)
        if (SLL_get_color(block->mem[i]) == SllWhite) {
          block->mem[i] = (Word)sll_free_cell[object_size];
          sll_free_cell[object_size] = &block->mem[i];
        } else {
          block->mem[i] = SLL_set_color(block->mem[i], SllWhite);
          live_heap_size += size_in_words;
        }
  }
  return live_heap_size;
}

void sll_gc_collect() {
  static size_t max_size = 0;
  if (heap_size <= max_size)
    return;
  gc_mark();
  size_t const live_heap_size = gc_sweep();
  size_t const next_max_size = (live_heap_size * 3) / 2;
  if (next_max_size > max_size)
    max_size = next_max_size;
}

void sll_fatal_error(char const *message) {
  fprintf(stderr, "SLL Fatal Error: %s\n", message);
  exit(EXIT_FAILURE);
}

Word *sll_allocate_object(size_t object_size) {
  sll_gc_collect();
  Word *const cell = sll_free_cell[object_size];
  if (cell) {
    sll_free_cell[object_size] = (Word *)cell[0];
    return cell;
  }

  struct Block *new_block = (struct Block *)malloc(sizeof(struct Block));
  if (!new_block)
    sll_fatal_error("Out of memory");
  new_block->next = sll_heap[object_size];
  sll_heap[object_size] = new_block;
  heap_size += SLL_BLOCK_SIZE;

  size_t const size_in_words = object_size + 1;
  size_t const block_size = (SLL_BLOCK_SIZE / size_in_words) * size_in_words;
  if (block_size > size_in_words)
    sll_free_cell[object_size] = &new_block->mem[size_in_words];
  for (size_t i = 2 * size_in_words; i < block_size; i += size_in_words)
    new_block->mem[i - size_in_words] = (Word)&new_block->mem[i];
  new_block->mem[block_size - size_in_words] = 0;
  return &new_block->mem[0];
}

void sll_print_value(Object const value, char const *const *ctr_names) {
  struct {
    struct RootsBlock header;
    Object value;
  } m = { { sll_roots, 1 }, value };
  sll_roots = &m.header;
  m.value = SLL_HEAD_FORM(m.value);
  CtrId const ctr_id = SLL_get_ctr_id(m.value[0]);
  size_t const size = SLL_get_osize(m.value[0]);
  if (ctr_id == SllExternalCtrId)
    printf("%s", (char const *)m.value[size + 1]);
  else
    printf("%s", ctr_names[ctr_id]);
  if (size)
    printf("(");
  for (size_t i = 1; i <= size; ++i) {
    if (i > 1)
      printf(", ");
    sll_print_value((Object)m.value[i], ctr_names);
  }
  if (size)
    printf(")");
  sll_roots = m.header.next;
}

static int lex_next(int skip_newline) {
  if (next_lexeme) {
    int const result = next_lexeme;
    next_lexeme = 0;
    return result;
  }
  int next_char = 0;
  do next_char = getchar();
  while (isspace(next_char) && (skip_newline || next_char != '\n'));
  if (next_char == '\n')
    return SllEof;
  switch (next_char) {
    case '(':
    case ')':
    case ',':
      return next_char;
    case EOF:
      return SllEof;
  }
  if (!isupper(next_char))
    sll_fatal_error("Unexpected symbol in standard input");
  int i = 0;
  for (; isalnum(next_char); ++i) {
    ctr_name_buf[i] = next_char;
    next_char = getchar();
  }
  ungetc(next_char, stdin);
  ctr_name_buf[i] = 0;
  return SllCtrName;
}

static inline int lex_look(int skip_newline) {
  if (next_lexeme)
    return next_lexeme;
  return next_lexeme = lex_next(skip_newline);
}

static inline void lex_take(int lexeme) {
  if (lexeme != lex_next(1))
    sll_fatal_error("Unexpected lexeme in standard input");
}

static inline int string_comp(void const *const lhs, void const *const rhs) {
  return strcmp(*(char const *const *)lhs, *(char const *const *)rhs);
}

static Object parse_value(char const *const *ctr_names, size_t numof_ctrs, int skip_newline) {
  static char const *const key = ctr_name_buf;
  lex_take(SllCtrName);
  char const *const *ctr_name_found = (char const *const *)
      bsearch(&key, ctr_names, numof_ctrs, sizeof(char *), string_comp);
  CtrId ctr_id = SllExternalCtrId;
  char const *ext_ctr_name = NULL;
  if (ctr_name_found)
    ctr_id = ctr_name_found - ctr_names;
  else
    ext_ctr_name = get_ext_ctr_name();

  size_t numof_args = 0;
  struct {
    struct RootsBlock header;
    Object args[SLL_MAX_OBJECT_SIZE];
  } m = { { sll_roots, SLL_MAX_OBJECT_SIZE } };
  sll_roots = &m.header;
  if (lex_look(skip_newline) == '(') {
    lex_take('(');
    if (lex_look(1) == SllCtrName) {
      m.args[numof_args++] = parse_value(ctr_names, numof_ctrs, 1);
      while (lex_look(1) == ',') {
        lex_take(',');
        m.args[numof_args++] = parse_value(ctr_names, numof_ctrs, 1);
      }
    }
    lex_take(')');
  }
  Word *const cell = sll_new_cell(numof_args + (ctr_id == SllExternalCtrId));
  cell[0] = SLL_make_header((Word)ctr_id, numof_args);
  for (size_t i = 0; i < numof_args; ++i)
    cell[i + 1] = (Word)m.args[i];
  if (ctr_id == SllExternalCtrId)
    cell[numof_args + 1] = (Word)ext_ctr_name;
  sll_roots = m.header.next;
  return cell;
}

Object sll_read_value(char const *vname, char const *const *ctr_names, size_t numof_ctrs) {
  printf("%s = ", vname);
  next_lexeme = 0;
  return parse_value(ctr_names, numof_ctrs, 0);
}

void sll_initialize() {
  init_ext_ctr_names(0x7);
}

void sll_finalize() {
  dispose_ext_ctr_names();
  for (size_t i = 0; i < SLL_MAX_OBJECT_SIZE; ++i) {
    struct Block *curr_block = sll_heap[i];
    while (curr_block) {
      struct Block *to_free = curr_block;
      curr_block = curr_block->next;
      free(to_free);
    }
  }
  printf("\n");
}
