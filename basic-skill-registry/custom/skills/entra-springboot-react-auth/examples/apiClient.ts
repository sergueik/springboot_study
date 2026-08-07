export async function callApi(
    url: string,
    token: string
) {
    return fetch(url, {
        headers: {
            Authorization: `Bearer ${token}`
        }
    });
}
