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
  pthread_barrier_t barrier;
  synch_t sync;
  struct proc_arg_t* prev;
} proc_arg_t;

void *init (void *arg){
  //unpack the arguments and get hold of the two placeholders
  int rounds = ((proc_arg_t*)arg) -> rounds;
  pthread_barrier_t barrier = ((proc_arg_t*)arg) -> barrier;
  pthread_mutex_t prev_mtxp = ((proc_arg_t*)arg) -> prev -> sync.mtx;
  pthread_mutex_t next_mtxp = ((proc_arg_t*)arg) -> sync.mtx;
  pthread_cond_t cnd = ((proc_arg_t*)arg) -> prev -> sync.cnd;
  int token = ((proc_arg_t*)arg) -> prev -> sync.token;
  int next_token = ((proc_arg_t*)arg) -> sync.token;

  printf("wait for everyone to sign in");

  //wait for everyone to sign in
  pthread_barrier_wait(&barrier);

  while(rounds > 0){
    //grab lock of previous
    pthread_mutex_lock(&prev_mtxp);

    //check if token is there, if not suspend on condition
    /*block this thread until another thread signal cond.
      while blocked, the mutex is released, then re-acquired before this thread is woken up and the call returns.
     */  
    if(!token) {
      pthread_cond_wait(&cnd, &prev_mtxp);
    }

    //remove the token and unlock previous

    if(token) { 
      token = 0; //set token as 0 equals false, thus removing the token    
     }
    pthread_mutex_unlock(&prev_mtxp); 

    //take mutex-lock and place token in next placeholder
    phtread_mutex_lock(&next_mtxp);
    next_token = 1;
    
    //unlock and signal on condition
    pthread_mutex_unlock(&next_mtxp);
    pthread_cond_signal(&cnd);

    rounds--;
  }
  pthread_barrier_wait(&barrier);
}


int benc(int p, int r){
  //int p = Number of threads
  //int r = number of rounds

  //set up the barrier
  pthread_barrier_t barrier;
  pthread_barrier_init(&barrier, NULL, p);

  //allocate some space for a table of thread arguments
  proc_arg_t *thrd;
  thrd = malloc(p * sizeof(proc_arg_t));
  
  //initialize the thread arguments
  pthread_mutex_t mtx;
  pthread_cond_t cnd;

  pthread_mutex_init(&mtx, NULL);
  pthread_cond_init(&cnd, NULL);
  
  for(int i = 0; i < p; i++) {
    *thrd[i].prev = thrd[(i - 1) % p];
    thrd[i].rounds = r;  
    thrd[i].sync.token = 0;
  }


  //take lock of one placeholder
  pthread_mutex_lock(&thrd[0].sync.mtx);       
  thrd[0].sync.token = 1;

  //create all threads
  printf("creating all threads");
  pthread_t *thrt = malloc(p * sizeof(pthread_t));      

  for(int i = 0; i < p; i ++){
     pthread_create(&thrt[i], NULL, init, &thrd[i]);
  }

  // take time
  //struct timeval start, stop;

  //getimeofday(&start, NULL);

  //let's wait for all to sign in
  pthread_barrier_wait(&barrier);

}


int main(int argc, char *argv[]) {

  struct timeval start, stop;
  
  printf("go to function benc");
  benc(5, 1);
  /*
    gettimeofday(&start, NULL);


    gettimeofday(&stop, NULL);

    int s = stop.tv_sec - start.tv_sec;
    int u = stop.tv_usec - start.tv_usec;
    int t = s*1000000+u;
    printf("%d\t%d\n", i, t);
  }
  */
  return 0;
}

