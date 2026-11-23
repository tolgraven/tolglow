#ifndef TOLGLOW_RUNTIME_H
#define TOLGLOW_RUNTIME_H

/* Opaque GraalVM isolate handles. The signed framework exports these C symbols. */
typedef struct graal_isolate graal_isolate_t;
typedef struct graal_isolatethread graal_isolatethread_t;
struct graal_create_isolate_params;
int graal_create_isolate(struct graal_create_isolate_params *, graal_isolate_t **, graal_isolatethread_t **);
int graal_tear_down_isolate(graal_isolatethread_t *);
int tolglow_open_local(graal_isolatethread_t *, const char *app_home, const char *show_folder);
int tolglow_close_local(graal_isolatethread_t *);
#endif
