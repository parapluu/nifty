struct list_item {
	struct list_item* next;
	int data;
};

typedef struct list_item* _usetype;

/* list functions */
typedef void ** list_t;

extern void   list_init(list_t list);
extern void * list_head(list_t list);
extern void * list_tail(list_t list);
extern void * list_pop (list_t list);
extern void   list_push(list_t list, void *item);

extern void * list_chop(list_t list);

extern void   list_add(list_t list, void *item);
extern void   list_remove(list_t list, void *item);

extern int    list_length(list_t list);

extern void   list_copy(list_t dest, list_t src);

extern void   list_insert(list_t list, void *previtem, void *newitem);

extern void * list_item_next(void *item);
