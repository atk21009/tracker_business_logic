tracker_business_logic

Running Eunits
    
    rebar3 eunit

Running Shell 

    rebar3 shell

### Replication Steps

In order to replicate clear db functionality one must create an env_variables.erl file with the following


auth_key() ->
    <<"Strong key">>.

With "Strong key" being a strong key that shouldn't be exposed. The strong key can be used in various places in the application to require authorization passphrase to allow certain actions to be performed.


### Package Data

    Package_id: string {
        location_id: string,
        delivered: bool,
        created: date
    }

### Package Delivered

    Package_id: string {
        location_id: null,
        delivered: true,
        created: date,
        delivered_at: date
    }

### Location

    Location_id: string {
        latitude: string || float,
        longitude: string || float
    }
