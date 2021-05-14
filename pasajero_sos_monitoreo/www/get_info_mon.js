$(document).on('shiny:connected', function(e) {
                 var v1 =  new Date();  
                 const options={
                  day:'numeric', 
                  month:'numeric',
                  year:'numeric'                    
		         }  
          
               Shiny.onInputChange('dtd', v1.toLocaleDateString('en-GB',options));
 $(document).on('shiny:inputchanged', function(event) {

    if (event.name === 'ala' ) {	
		var v2 =  new Date();
	   const options_time={
		hour12:false,
		hour:'2-digit',
		minute:'2-digit',
		second:'2-digit'
		}
	Shiny.onInputChange("ttd",v2.toLocaleTimeString('en-GB',options_time));
    }
  });
  
  
  
              });