foomaker <- function(foo){
    if (foo == 1){
        print("Done")
    } else {
        bar()
        bar()
        bar()
        bar()
    }
}

bar <- function(){
    print("Wocka Wocka Wocka")
}