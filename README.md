# Postgresql driver

## Installation

### Download

    $ git clone git@github.com:francesco-bracchi/postgresql-gambit.git

### Compile

    $ cd postgresql-gambit
    $ make

### Install

    $ sudo make install
    

## Use

### Example

    (with-connection 
     (list database: "db_name"
           username: "db_user"
           password: "db_pass")
      (lambda () 
       (connection-execute 
        "SELECT key, val FROM key_value_table WHERE key like $1"
		arguments: (list "%a%")
        initial-value: '()
        reducer: (lambda (state key value) (cons (cons key value) state)))))

the whole expression evaluates to an alist containing rows in `key_value_table` table
where key contains the letter `A`
