tracker_business_logic

Running Eunits
    
    rebar3 eunit

Running Shell 

    rebar3 shell

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
