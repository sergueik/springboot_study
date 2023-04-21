function () {
    var key = new Date(
        this.occuredOn.getFullYear(),
        this.occuredOn.getMonth(),
        this.occuredOn.getDate(),
        this.occuredOn.getHours(),
        this.occuredOn.getMinutes(),
        0, 0)

    var value = {
        total: this.value,
        count: 1,
        min: this.value,
        max: this.value,
        mean: 0,
        ts: new Date()
	};
                
    emit(key, value);

};