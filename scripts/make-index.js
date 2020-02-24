/**
 * Generates the complete list of the posts before the commit to the gh-pages branch.
 */

const MAX_LINES = 10;

const fs = require('fs');

const dir = fs.readdirSync('./public/posts');
const postList = dir.filter(postName => postName !== 'index');

const result = postList.map(getPostData).filter(postData => postData !== null);
result.sort((a, b) => {
    if (b.date !== a.date) {
        return b.date - a.date;
    }
    return b.mtime.mtime - a.mtime.mtime;
});

const postData = result.map(
    ({ slug, title, date, summary, category }) => ({ slug, title, date: new Date(date).toISOString().substring(0, 10), summary, category })
);
console.info(postData.map(({ slug, title, date }) => ({ slug, title, date })));

fs.writeFileSync('./public/posts/index', JSON.stringify(postData));

/**
 * Get the data about a single post:
 * the slug, the title, the date, and the summary (the first few lines from the post)
 * @param {string} post The path of the post (without prefix `./public/posts`)
 */
function getPostData(post) {
    const fileContent = fs.readFileSync(`./public/posts/${post}`, 'utf8');

    const [, yamlString, markdownString] = fileContent.split('---\n').map(s => s.trim());
    const yaml = Object.fromEntries(
        yamlString
            .trim()
            .split('\n')
            .map(s => [
                s.slice(0, s.indexOf(':')).trim(),
                s.slice(s.indexOf(':') + 1).trim(),
            ]),
    );

    if (!yaml.title || !yaml.date || !yaml.category) {
        console.warn(`Appropriate data are not given in the post '${post}'.`);
        return null;
    }

    yaml.category = yaml.category[0] === '['
        ? yaml.category.slice(1, -1).split(',').map(s => s.trim())
        : [yaml.category];

    const splittedMarkdown = markdownString.split(/(\n+)/);
    let summaryLines;
    {
        let i = 0;
        let numberOfLines = 0;
        while (i < splittedMarkdown.length && numberOfLines < MAX_LINES) {
            if (splittedMarkdown[i].trim()) {
                numberOfLines++;
            }
            i++;
        }
        summaryLines = splittedMarkdown.slice(0, i);
    }
    let summary = summaryLines.join('');
    if (countInstances(summary, '```') % 2 === 1) {
        summary += '\n```';
    }
    if (countInstances(summary.replace(/\\\$/g, ''), '$$') % 2 === 1) {
        summary = summary.slice(0, summary.lastIndexOf('$$')).trim();
    }

    const date = Date.parse(yaml.date);
    if (isNaN(date)) {
        console.warn(`Appropriate date is not given in the post '${post}'.`);
        return null;
    }

    return {
        slug: post.replace('.md', ''),
        title: yaml.title,
        date,
        summary,
        category: yaml.category,
        mtime: fs.statSync(`./public/posts/${post}`),
    };
}

function countInstances(string, word) {
    return string.split(word).length - 1;
}

