#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <pthread.h>

typedef struct synch_t {
  int token;
  pthread_mutex_t mtx;
  pthread_cond_t cnd;
} synch_t;

typedef struct proc_arg_t {
  int rounds;
  synch_t sync;
  struct proc_arg_t* prev;
  struct proc_arg_t* next;
  int id;
} proc_arg_t;

struct timespec start, stop;

pthread_barrier_t barrier;

void *init(void *arg){
  int rounds = ((proc_arg_t*)arg) -> rounds;
  int id = ((proc_arg_t*)arg) ->  id;
  int prev_id = ((proc_arg_t*)arg) -> prev -> id;
  int next_id = ((proc_arg_t*)arg) -> next -> id;

  pthread_mutex_t prev_mtxp = ((proc_arg_t*)arg) -> prev -> sync.mtx;
  pthread_mutex_t mtxp = ((proc_arg_t*)arg) -> sync.mtx;
  pthread_cond_t cnd = ((proc_arg_t*)arg) -> sync.cnd;
  pthread_cond_t next_cond = ((proc_arg_t*)arg) -> next -> sync.cnd;


  int* prev_token = &((proc_arg_t*)arg) -> prev -> sync.token;
  int* next_token = &((proc_arg_t*)arg) -> sync.token;
  
  //printf("My id is: %d, my prev is: %d, my next is: %d and my token is %d\n", id, prev_id, next_id, *next_token);

  //wait for everyone to sign in
  pthread_barrier_wait(&barrier);

  while(rounds > 0){
  //grab lock of prev process
    pthread_mutex_lock(&prev_mtxp);

    //check if next_token is there, if not suspend on condition
    while(*prev_token == 0) {
      pthread_cond_wait(&cnd, &prev_mtxp);
      pthread_mutex_unlock(&prev_mtxp);
    }

    if(*prev_token == 1) {
      *prev_token = 0;
    }

    pthread_mutex_unlock(&prev_mtxp);

    //take mutex-lock and place next_token in next placeholder
    pthread_mutex_lock(&mtxp);
    *next_token = 1;

    //unlock and signal on condition
    pthread_cond_signal(&next_cond);
    pthread_mutex_unlock(&mtxp);

    rounds--;
  }

  pthread_barrier_wait(&barrier);
}

static double converterToSeconds(struct timespec *ts){
    return (double)ts->tv_sec + (double)ts->tv_nsec / 1000000000.0;
};

void bench(int p, int r){

  //set up the barrier
  pthread_barrier_init(&barrier, NULL, p);


  //allocate some space for a table of thread arguments
  proc_arg_t *thrd;
  thrd = (proc_arg_t*)malloc(p * sizeof(proc_arg_t));

  for(int i = 0; i < p; i++) {
    thrd[i].rounds = r;
    thrd[i].sync.token = 0;
    thrd[i].id = i;
    thrd[i].prev = &thrd[(i - 1 + p) % p];
    thrd[i].next = &thrd[(i + 1) % p];
    pthread_cond_init(&thrd[i].sync.cnd, NULL);
    pthread_mutex_init(&thrd[i].sync.mtx, NULL);
  }

  thrd[0].sync.token = 1;

  //create all threads
  pthread_t *thrt = malloc(p * sizeof(pthread_t));


  for(int i = 0; i < p; i++){
      if(i == p - 1) {   clock_gettime(CLOCK_MONOTONIC, &start); }
     pthread_create(&thrt[i], NULL, &init, &thrd[i]);
  }

  for(int i = 0; i <p; i++){
    pthread_join(thrt[i], NULL);
  }

  clock_gettime(CLOCK_MONOTONIC, &stop);
  double elapsedTime = converterToSeconds(&stop) - converterToSeconds(&start);
  printf("Elapsed time: %f", elapsedTime);
  
}

int main() {

  bench(5, 2);
  return 0;
}



