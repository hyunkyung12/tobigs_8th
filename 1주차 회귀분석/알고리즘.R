# 1번 문제 : 반복문(for, while)을 사용하지 않고 i 번째 피보나치 수를 출력하는 함수를 구현하세요.

fibonacci_number <- function(i)

{

  if(i==1){ # 첫번째 피보나치수는 0

    return(0)

  }else if(i==2){ # 두번째 피보나치수는 1

    return(1)

  }else{

    return(fibonacci_number(i-1)+fibonacci_number(i-2)) # 세번째 부터는 앞, 그 앞의 두 수의 피보나치 값을 더합니다

  }

}

fibonacci_number(3)

  

# 2번 문제 : 첫번째 피보나치 수부터 n번째 피보나치 수까지의 합을 result 변수에 담아주세요! 

#           단, 0 < n <= 10000, 제한 시간 1초

system.time({

  rm(list = ls())

  sum_of_fibonacci <- function(n)

  {

    a=0;b=1;sum=0

    for(i in 1:n){

      sum = sum+a

      c = a+b;

      a=b;

      b=c;

    }

    return(sum%% 1000000009)

  }

  sum_of_fibonacci(sample(1000:10000, 1))

})



# 3번 문제 : my_function은 정수 n(1<=n<=100000)을 입력받아 세 가지 변환 여러번을 통해 최종적으로 1을 만드는데, 그때 변환의 최소 수를 출력하는 함수다.

#            변환의 방법은 다음과 같다.

#               1. n이 3의 배수 인 경우 -> n / 3으로 변환

#               2. n이 2의 배수 인 경우 -> n / 2으로 변환

#               3. n - 1로 변환

#            만약 n이 6이라면, 3의 배수이기 때문에, 1번 변환에 의해 2가 될 수도 있고, 2번 변환에 의해 3이 될 수도 있고, 3번 변환에 의해 5가 될 수도 있다.

#            그리고 6을 1번 변환을 통해 2가되면 2, 3번 변환 중 둘 중 하나를 거치면 1이 된다.

#            즉, my_function(6) = 2가 된다.

#            my_function을 구현하세요. 제한 시간 1초

system.time({

  rm(list = ls())

  my_function <- function(n)

  {

    result <- rep(0,n) # n개짜리 벡터 생성 



                   # base case 지정

    result[1] <- 0 # 1을 1로 만드는데는 0번

    result[2] <- 1 # 2를 1로 만드는데는 1번

    result[3] <- 1 # 3을 1로 만드는데는 1번

    

    for(i in 4:n){ # 4 부터 시작

      result[i] <- result[i-1] +1 # 기본 : 전의 경우에서 1번 더하는 형식



      if(i%%2==0 && result[i] > result[i/2]+1){ # i가 2의 배수이고, 기본경우로 하는것 보다는 -1해서 2로 나누는게 빠른 경우

        result[i] <- result[i/2] +1 # 빠른경우로 교체

      }

      if(i%%3==0 && result[i] > result[i/3]+1){ # 2의 경우와 동일

        result[i] <- result[i/3] +1 

      }

    }

    return(result[n])

  }

  my_function(sample(10000:100000, 1))

})