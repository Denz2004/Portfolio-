fetch('/Users/deniz.ozayaz/Desktop/DS105_Proj/ds105w-project-my-team/reddit_data.json')
.then(function(response){
    return response.json();
})
.then(function(posts){
    let placeholder = document.querySelector('#data-output');
    let out = "";
    for (let post of posts){
        out += `
            <tr>
                <td>${post.subreddit}</td>
                <td>${post.title}</td>
                <td>${post.id}</td>
                <td>${post.upvote_ratio}</td>
            </tr>
        `;
    } 
    placeholder.innerHTML = out;
})
