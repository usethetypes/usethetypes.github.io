$(() => {
  function makeVideoUrl(videoId) {
    return `https://youtu.be/${videoId}`;
  }

  function makeImageUrl(videoId) {
    return `https://img.youtube.com/vi/${videoId}/0.jpg`;
  }

  function makeNoEmbedUrl(videoId) {
    return `https://noembed.com/embed?url=https://www.youtube.com/watch?v=${videoId}`;
  }

  const videoIds = [
    "iNeLpmjowwQ",
    "cDYn_la-9vg",
    "hp-uQZ-MujA",
    "uB7ES5-JhQA",
    "vcE9BlW74tg",
    "sP6Iuwd_Nik",
    "9qiRk5la724"
  ];

  const divContainer = $("#carousel .carousel-inner");
  const divTemplate = $("#carousel-template-image");

  for (let i = 0; i < videoIds.length; ++i) {
    const videoId = videoIds[i];

    const div = $(divTemplate.html());
    div.attr("id", `video-${videoId}`);

    const anchor = div.find("> :first-child");
    anchor.attr("href", makeVideoUrl(videoId));

    const image = anchor.find("> :first-child");
    image.attr("src", makeImageUrl(videoId));

    if (i == 0) {
      div.addClass("active");
    }

    divContainer.append(div);

    $.get({
      url: makeNoEmbedUrl(videoId),
      dataType: "json"
    }).then(response => {
      const slug = `testvideo#${i}`;
      const anchorTitle = `${slug}: ${response.title}`;
      anchor
        .attr("title", anchorTitle)
        .attr("alt", anchorTitle);
      div.find("h5").text(response.title);
      div.find("p").text(slug);
    });
  }
});
