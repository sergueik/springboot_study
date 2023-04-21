function () {
    var key = new Date(
        this._id.getFullYear(),
        this._id.getMonth(),
        0, 0, 0, 0, 0)

    var value = {
        total: this.value.total,
        count: this.value.count,
        min: this.value.min,
        max: this.value.max,
        mean: 0,
        ts: new Date()
	};
                
    emit(key, value);

};