
//
// Show/hide some sections by clicking on the title.
//
// Can be replaced by <details>, <summary> once it is supported by most browsers.
//

function saveStatus(id, shown) {
    if (typeof(Storage) !== "undefined") {
	localStorage.setItem("yfold-shown-" + id, shown) ;
    }
}

// record : (when clicked), record the status in local storage
function setStatus(id, shown, content, record) {

    // console.log ("setStatus (" + id + ", " + shown + ")") ;
    
    var span = document.getElementById("arrow-" + id) ;
    
    if (shown) {
	content.classList.remove("anim-hide") ;
        content.classList.remove("hidden") ;
        span.innerHTML = "&#x25be;" ;
        if (record) { content.classList.add("anim-show") ; }
        else { content.classList.add("shown") ; }
    }
    else {
        content.classList.remove("anim-show") ;
        content.classList.remove("shown") ;
        span.innerHTML = "&#x25b8;" ;
        if (record) { content.classList.add("anim-hide") ; }
        else { content.classList.add("hidden") ; }
    }

    if (record) { saveStatus(id, shown) ; }
}

// Invoked at load-time once for every yfold section.
function initYfold(id) {

    // console.log ("initYfold (" + id + ")") ;

    // console.log ("unknown " + id)
    var shown = true ;
    var content = document.getElementById("content-" + id) ;
    
    // Sets the state according to local storage or default value.
    
    if (typeof(Storage) !== "undefined") {
	var local = localStorage.getItem("yfold-shown-" + id) ;
	// console.log ("using storage value = " + local)
	
	if (local === 'true') {
	    shown = true ;
	}
	else if (local === 'false') {
	    shown = false ;
	}
	else {
	    // Get default value
	    var defv = content.getAttribute("data-yfold-default") ;
	    if (defv === 'hide') {
		shown = false ;
	    }
	    // console.log ("using default value = " + shown)
	}
    }
    
    setStatus(id, shown, content, false) ;
}

// Function invoked when the title is clicked.
function toggleYfold(id) {

    // console.log ("toggleYfold (" + id + ")") ;
    
    var content = document.getElementById("content-" + id) ;
    var expand = content.classList.contains("anim-hide") || content.classList.contains("hidden") ;
    setStatus(id, expand, content, true) ;
}
