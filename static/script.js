// Global data struct
var data = {
    dnsmasq: [],
};

// Update data
var update_data = function () {
    axios
        .get('/api/v1/dnsmasq/')
        .then(response => data.dnsmasq = response.data)
        .catch(error => console.error(error));
};

window.onload = function () {
    // Vue App
    new Vue({
        el: "#app",
        data: data,
        created: update_data,
    });
    // Trigger next refresh after set time if over.
    const refresh = new URLSearchParams(window.location.search).get('refresh');
    if (refresh) {
        setInterval(function () {
            update_data();
        }.bind(this), refresh * 1000);
    }
};
