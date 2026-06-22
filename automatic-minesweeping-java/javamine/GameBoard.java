package cs305.minesweeper.javamine;

import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Scanner;

import cs305.minesweeper.MinesweeperBoard;
import cs305.minesweeper.MinesweeperTile;
import cs305.minesweeper.MinesweeperTile.TileState;

/**
 * Minesweeper board for cs305 project
 * @author Walter Seme
 *
 */
public class GameBoard implements MinesweeperBoard {

	private GameTile[][] board;
	private GameState gameState;
	
	private int totalColumns;
	private int totalRows;
	private int totalBombs;
	
	private int bombsLeft; 
	
	private Dimension boardSize;
	private MinesweeperGui gui;
	
	/**
	 * constructs the game board with given size and amount of bombs
	 * @param row
	 * @param column
	 * @param bombAmount
	 */
	public GameBoard(int row, int column, int bombAmount){

		if(bombAmount > row*column || bombAmount <= 0){
			System.err.println("Did not allocate bombs on board");
			System.err.println("Make sure bomb amount to be less than " + row*column  + " and greater than 0");
			return;
		}else{
			gameState = GameState.Waiting; //starts state in waiting until user clicks somewhere
			totalColumns = column;
			totalRows = row;
			bombsLeft = bombAmount;
			totalBombs = bombAmount;

			//create board according to size
			board = new GameTile[row][column];//creates board for values and bombs

			//initializes each tile, no null values, also clears it
			clearBoard();

			//Dimension size of the board
			boardSize = new Dimension();
			boardSize.setSize(row,column);

			gui = new MinesweeperGui(this);
		}

	}

	/**
	 * assigns each tile a value, needs to implement true or false first
	 */
	private void setTileValues(){

		//goes through each value in board
		for (int y = 0; y < totalColumns; y++) {
			for (int x = 0; x < totalRows; x++) {
				//not pretty
				//goes through each surrounding tile
				for (int yIndex = y - 1; yIndex <= y + 1; yIndex++) {
					for (int xIndex = x - 1; xIndex <= x + 1; xIndex++) {

						// makes sure tile is valid and if value should be incremented, also not pretty but works
						if ((yIndex >= 0) && (yIndex < totalColumns) && (xIndex >=0) && (xIndex < totalRows)) {
							if (board[xIndex][yIndex] != board[x][y]&& board[xIndex][yIndex].getValue() == MinesweeperTile.BOMB_VALUE && board[x][y].getValue() != MinesweeperTile.BOMB_VALUE)
								board[x][y].incrementValueByOne();//increment value of surrounding bomb
						}
					}
				}
			}
		}
	}

	/**
	 * populates all of the tile's surrounding tiles in attribute
	 * call after everything is set
	 */
	private void setSurroundingTilesOfEachTile(){
		for (int y = 0; y < totalColumns; y++) {
			for (int x = 0; x < totalRows; x++) {
				//since surrounding tiles can be different amount, used arraylist 
				ArrayList<GameTile> surroundingTiles = new ArrayList<GameTile>();

				//not pretty
				//goes through each surrounding tile and stores it in arraylist
				for (int yIndex = y - 1; yIndex <= y + 1; yIndex++) {
					for (int xIndex = x - 1; xIndex <= x + 1; xIndex++) {

						// makes sure value is not null or is current tile
						if ((yIndex >= 0) && (yIndex < totalColumns)
								&& (xIndex >= 0) && (xIndex < totalRows)) {
							if (board[xIndex][yIndex] != null
									&& board[xIndex][yIndex] != board[x][y])
								surroundingTiles.add(board[xIndex][yIndex]);
						}
					}
				}
				board[x][y].setSurroundingTiles(surroundingTiles);//sets surrounding tiles in tile	
			}
		}	
	}

	/**
	 * randomly places the bombs on the board
	 * @param row
	 * @param column
	 */
	private void placeBombsRandomly(MinesweeperTile noMineTile){
		
		int b = 0;
		while(b < bombsLeft){
			//gets random coordinates
			int xRand =  (int)(Math.random() * totalRows) ;
			int yRand = (int)(Math.random() * totalColumns);

			//places bomb if it is not one already
			if(board[xRand][yRand].getValue() != MinesweeperTile.BOMB_VALUE && board[xRand][yRand] != noMineTile){
				board[xRand][yRand].setToBomb();
				b++;
			}
		}	
	}

	/**
	 * exposes emptyValues and exposes tiles surrounding empty tiles
	 * used in ClickTile()
	 * does not expose flagged tiles
	 * uses recursion
	 * @param gameTile
	 */
	private void exposeSurroundingTiles(GameTile gameTile){

		MinesweeperTile[] mst = gameTile.getSurroundingTiles();
		GameTile[] tiles = (GameTile[])mst;//casts to GameTile so state can be set

		for(int i = 0; i< tiles.length;i++){
			//only exposes if in hidden (like game) and value is empty, does not expose flagged
			if(tiles[i].getValue() == MinesweeperTile.EMPTY_VALUE  && tiles[i].getState() == TileState.Hidden && tiles[i].getState() != TileState.Flagged){
				tiles[i].setState(TileState.Exposed);
				exposeSurroundingTiles(tiles[i]);//expose those values surrounding an empty, recursion
			}
			//does not expose flags
			else if ( tiles[i].getState() != TileState.Exposed && tiles[i].getState() != TileState.Flagged){
				//set values to exposed if next to empty
				tiles[i].setState(TileState.Exposed);
			}
		}
	}

	/**
	 * checks if player won the game
	 */
	private void checkWin(){
		int checkWin = 0;// counter for checks
		boolean allExposed = true;
		// check if you win the game
		if (bombsLeft <= 0) {

			for (MinesweeperTile mt : getAllTiles()) {
				// checks to see if all bombs are flagged
				if (mt.getValue() == MinesweeperTile.BOMB_VALUE
						&& mt.getState() == TileState.Flagged) {
					checkWin++;
				}
				//checks if each tile is exposed
				if (mt.getState() == TileState.Hidden && allExposed != false){
					allExposed = false;
				}
			}

			if (checkWin == totalBombs && allExposed == true ) {
				gameState = GameState.Won;// You win
			}

		}

	}

	/**
	 * sets the bombs, and values of the board
	 */
	private void generateBoard(MinesweeperTile clickedTile){
		
		placeBombsRandomly(clickedTile);
		
		//sets each tile a value
		setTileValues();

		//assigns each tile a surrounding tile
		setSurroundingTilesOfEachTile();
		
		gui.updateBoard(getAllTiles());
		
	}

	/**
	 * exposes all values after a lost
	 */
	private void exposeTiles(){
		//has to be in lost state
		if (getGameState() == GameState.Lost) {
			for (int y = 0; y < totalColumns; y++) {
				for (int x = 0; x < totalRows; x++) {
					if (board[x][y].getState() != TileState.Exposed) {
						board[x][y].setState(TileState.Exposed);
					}
				}
			}
			gui.updateBoard(getAllTiles());
		}
	}
	
	/**
	 * clears the board, no null values
	 */
	private void clearBoard(){
		for (int y = 0; y < totalColumns; y++) {
			for (int x = 0; x < totalRows; x++) {
				board[x][y] = new GameTile();//creates a tile
			}
		}
	}
	
	/**
	 * prints all states to user and values if exposed, shortened
	 */
	public void printTileStates(){

		System.out.println("");
		for (int y = 0; y < totalColumns; y++) {
			for (int x = 0; x < totalRows; x++) {
				//if exposed prints value, else says hidden
				if(board[x][y].getState() == TileState.Exposed){
					if(board[x][y].getValue() == MinesweeperTile.BOMB_VALUE){
						System.out.print("B ");
					}else{
						System.out.print(board[x][y].getValue() + " ");
					}
				}else if(board[x][y].getState() == TileState.Hidden)
					System.out.print("H ");
				else if (board[x][y].getState() == TileState.Flagged)
					System.out.print("F ");
			}
			System.out.println("");
		}
	}
	
	/**
	 * clicks inputed tile
	 */
	@Override
	public void clickTile(MinesweeperTile tile) {
		//check for null
		if(tile == null)
			return;

		//can not click a flagged tile
		if(tile.getState() == TileState.Flagged)
			return;
		
		//generates board after first click, prevents loosing on first click
		if (gameState == GameState.Waiting){
			generateBoard(tile);//does not allow this tile to have a value of BOMB_VALUE
			gameState = GameState.Playing;
		}
		
		GameTile gt = (GameTile)tile;//need to cast to GameTile to use setState(State)

		//checks if you can click on a tile
		if(gt.getState() == TileState.Hidden && gameState != GameState.Lost || gameState != GameState.Won){
			
			gt.setState(TileState.Exposed); //show tile
			
			if(gt.getValue() == MinesweeperTile.BOMB_VALUE){
				gameState = GameState.Lost;
				exposeTiles();//you lose
			}
			else if(gt.getValue() == MinesweeperTile.EMPTY_VALUE){
				//get surrounding values, expose them if they are a value, if 0 stop
				exposeSurroundingTiles(gt);//works!
			}
		}
		
		gui.updateBoard(getAllTiles());
		checkWin();
	}
	
	/**
	 * Overloads clickTile, calls clickTile(MinesweeperTile tile)
	 */
	@Override
	public void clickTile(int x, int y) {
		//check if valid location
		if(x >= 0 && x <totalRows && y >= 0 && y < totalColumns){
			clickTile(board[x][y]);//Overloads		
		}
	}
	
	/**
	 * when called, flags or unflags a tile when hidden
	 */
	@Override
	public void flagTile(MinesweeperTile tile) {
		GameTile gt = (GameTile) tile;
		// now playing the game
		if (gameState == GameState.Waiting)
			gameState = GameState.Playing;

		//only works if game is being played
		if (gameState == GameState.Playing) {
			if (gt.getState() == TileState.Hidden) {
				gt.setState(TileState.Flagged);

				bombsLeft--;
			} else if (gt.getState() == TileState.Flagged) {
				gt.setState(TileState.Hidden);
				bombsLeft++;
			}
			
			//checks if board won
			checkWin();
			gui.updateBoard(getAllTiles());
			gui.setMinesLeftLabel(bombsLeft);
		}
	}

	/**
	 * overloads flagTile(MinesweeperTile tile)
	 */
	@Override
	public void flagTile(int x, int y) {
		flagTile(board[x][y]);
	}

	/**
	 * returns all tiles from the board
	 */
	@Override
	public MinesweeperTile[] getAllTiles() {

		MinesweeperTile[] AllTiles = new MinesweeperTile[totalColumns* totalRows];
		int i = 0;
		for (int y = 0; y < totalColumns; y++) {
			for (int x = 0; x < totalRows; x++) {
				AllTiles[i] = board[x][y];
				i++;
			}
		}
		return AllTiles;
	}

	/**
	 * returns all the hidden tiles on the board
	 * interface defines
	 */
	@Override
	public MinesweeperTile[] getHiddenTiles() {
		
		int numberOfUnknowns = 0;
		for (int y = 0; y < totalColumns; y++) {
			for (int x = 0; x < totalRows; x++) {
				if(board[x][y].getState() == TileState.Hidden)
					numberOfUnknowns++;
			}
		}

		MinesweeperTile[] unknown = new MinesweeperTile[numberOfUnknowns];
		//could of used array list, but decided to just go through it twice, not sure which is better
		int i = 0;
		for (int y = 0; y < totalColumns; y++) {
			for (int x = 0; x < totalRows; x++) {
				if(board[x][y].getState() == TileState.Hidden){
					unknown[i] = board[x][y];
					i++;
				}
			}
		}
		return unknown;
	}

	/**
	 * returns the Dimension of the size of the board
	 */
	@Override
	public Dimension getBoardSize() {
		return boardSize;
	}

	/**
	 * returns the game state of the board
	 */
	@Override
	public GameState getGameState() {
		return gameState;
	}

	/**
	 * returns the bombs left to be flagged on the board
	 * @return
	 */
	public int getBombsLeft(){
		return bombsLeft;
	}

	/**
	 * returns tile at x, y coordinates
	 */
	@Override
	public MinesweeperTile getTile(int x, int y) {
		return board[x][y];
	}

	/**
	 * returns all the exposed, Number tiles that are not empty or a bomb
	 */
	@Override
	public MinesweeperTile[] getNumberTiles() {

		int numberOfNumberTiles = 0;
		
		//gets amount of exposed tiles
		for (int y = 0; y < totalColumns; y++) {
			for (int x = 0; x < totalRows; x++) {
				if(board[x][y].getState() == TileState.Exposed)
					if(board[x][y].getValue() > 0 && board[x][y].getValue() < 9)
						numberOfNumberTiles++;
			}
		}
		
		MinesweeperTile[] numbers = new MinesweeperTile[numberOfNumberTiles];
		
		int i = 0;
		
		//stores tiles into array for each one exposed
		for (int y = 0; y < totalColumns; y++) {
			for (int x = 0; x < totalRows; x++) {
				if(board[x][y].getState() == TileState.Exposed)
					if(board[x][y].getValue() > 0 && board[x][y].getValue() < 10){
						numbers[i] = board[x][y];
						i++;
					}
			}
		}
		
		return numbers;
	}

	/**
	 * user decides when the board should be displayed
	 * Larry wanted it like that for his code
	 */
	@Override
	public void displayBoard() {
		gui.displayGui();
	}

	/**
	 * resets the board if user wants to play a new game
	 * saves memory instead of creating a new instance
	 */
	@Override
	public void resetBoard() {
		gui.hideBoard(getAllTiles());
		clearBoard();
		bombsLeft = totalBombs;
		gameState= GameState.Waiting;
		gui.setMinesLeftLabel(totalBombs);
		
	}
	
	/**
	 * main method, used for unit testing purposes
	 * @param args
	 */
	public static void main(String[] args){

		int x = 9;
		int y = 9;
		int mines = 10;


		GameBoard gm = new GameBoard(x, y , mines );
		gm.getNumberTiles();

		Scanner scan = new Scanner(System.in);
		gm.printTileStates();
		while (true){

			System.out.println("There are " + gm.bombsLeft + " bombs left to flag");
			System.out.println("Enter an x and y coordinate and c for click or f for flag (x y c): ");
			int xClick = scan.nextInt();
			int yClick = scan.nextInt();
			String action = scan.next();

			if(action.equalsIgnoreCase("c")){
				gm.clickTile(xClick, yClick);	
			}else if(action.equalsIgnoreCase("f")){
				gm.flagTile(xClick, yClick);
			}

			if(gm.gameState != GameState.Won && gm.gameState != GameState.Lost)
				gm.printTileStates();
				
			
			gm.displayBoard();
		}
		
	}
}
