const images = [
    'image1.jpg',
    'image2.jpg',
    'image3.jpg',
    'image4.jpg'
];

var currentIndex = 0;

const imageElement = document.getElementById('image');
const prevBtn = document.getElementById('prevBtn');
const nextBtn = document.getElementById('nextBtn');

function update()
{
    imageElement.src = images[currentIndex];
}

prevBtn.addEventListener('click', ()=>
{
    if(currentIndex>0 && currentIndex<=3)
    currentIndex--;
    else currentIndex = 3;
    update();
})

nextBtn.addEventListener('click', ()=>
{
    if(currentIndex>=0 && currentIndex<3)
    currentIndex++;
    else currentIndex = 0;
    update();
})

////////////////////////

var modal = document.getElementById("myModal");
var btnOpen = document.getElementById("openModal");
var btnClose = document.getElementById("closeModal");


btnOpen.addEventListener('click', function() {
    modal.style.display = "block";
});

btnClose.addEventListener('click', function() {
    modal.style.display = "none";
});



